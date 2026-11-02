module Common.Pages.Users.Edit.Profile exposing
    ( Model
    , Msg
    , fetchData
    , footer
    , initialModel
    , setUser
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Models.User exposing (User)
import Common.Api.Models.UserIdentity as UserIdentity exposing (UserIdentity)
import Common.Api.Roles as RolesApi
import Common.Api.Users as UsersApi
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.Markdown as Markdown
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.UserEditForm as UserEditForm exposing (UserEditForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.TimeUtils as TimeUtils
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.DescriptionList as DescriptionList
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Text as Text



-- MODEL


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , user : ActionResult User
    , roles : ActionResult (List Role)
    , savingUser : ActionResult String
    , userForm : Form FormError UserEditForm
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , user = Loading
    , roles = Loading
    , savingUser = Unset
    , userForm = UserEditForm.initEmpty
    }


fetchData : AppState -> UuidOrCurrent -> Cmd Msg
fetchData appState uuidOrCurrent =
    if UuidOrCurrent.isCurrent uuidOrCurrent then
        Cmd.none

    else
        RolesApi.getRoles appState GetRolesCompleted


setUser : User -> Model -> Model
setUser user model =
    { model
        | user = Success user
        , userForm = UserEditForm.init user
    }



-- UPDATE


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError User)
    | Discard
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        EditFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.userForm ) of
                ( Form.Submit, Just userForm ) ->
                    let
                        body =
                            UserEditForm.encode model.uuidOrCurrent userForm
                    in
                    ( { model | savingUser = Loading }
                    , Cmd.map cfg.wrapMsg (UsersApi.putUser appState model.uuidOrCurrent body PutUserCompleted)
                    )

                _ ->
                    ( { model | userForm = Form.update UserEditForm.validation formMsg model.userForm }
                    , Cmd.none
                    )

        Discard ->
            -- The bar's buttons go with the bar, so focus would be left on
            -- nothing: it goes to the page's heading, as it does in settings.
            ( { model
                | userForm = ActionResult.unwrap model.userForm UserEditForm.init model.user
                , savingUser = Unset
              }
            , Cmd.map cfg.wrapMsg (Section.focusHeading NoOp)
            )

        NoOp ->
            ( model, Cmd.none )

        GetRolesCompleted result ->
            case result of
                Ok pagination ->
                    ( { model | roles = Success pagination.items }, Cmd.none )

                Err error ->
                    ( { model | roles = ApiError.toActionResult appState (gettext "Unable to get the roles." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PutUserCompleted result ->
            case result of
                Ok user ->
                    let
                        refreshCmd =
                            if isCurrentUser appState user then
                                CommonForm.reloadWhenSaved

                            else
                                Cmd.none
                    in
                    -- The form is set to what was saved: while it still
                    -- differs from what was loaded, the bar goes on offering to
                    -- save what is already saved. Saving does not touch the
                    -- connected accounts, so the loaded ones stay.
                    ( { model
                        | savingUser = Success (gettext "Profile was successfully updated." appState.locale)
                        , user = Success { user | externalIdentities = ActionResult.unwrap user.externalIdentities .externalIdentities model.user }
                        , userForm = UserEditForm.init user
                      }
                    , refreshCmd
                    )

                Err error ->
                    ( { model
                        | savingUser = ApiError.toActionResult appState (gettext "Profile could not be saved." appState.locale) error
                        , userForm = FormUtils.setFormErrors appState error model.userForm
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if ActionResult.isSuccess model.user then
        CommonForm.submitShortcut appState { result = model.savingUser, onSubmit = EditFormMsg Form.Submit }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    let
        rolesActionResult =
            if UuidOrCurrent.isCurrent model.uuidOrCurrent then
                Success []

            else
                model.roles
    in
    Section.view appState
        { title = gettext "Profile" appState.locale
        , guideLink = Nothing
        , content = Section.actionResult appState (userView appState model) (ActionResult.combine model.user rolesActionResult)
        }


userView : AppState -> Model -> ( User, List Role ) -> List (Html Msg)
userView appState model ( user, roles ) =
    [ Layout.split
        [ userFormView appState model roles (UuidOrCurrent.isCurrent model.uuidOrCurrent)
        , Layout.stackLoose
            (Layout.stack
                [ Text.label (gettext "User Image" appState.locale)
                , UserAvatar.user
                    { firstName = user.firstName
                    , lastName = user.lastName
                    , imageUrl = user.imageUrl
                    , gravatarHash = UserAvatar.emailHash user.email
                    }
                    |> ItemIcon.withSize ItemIcon.Large
                    |> ItemIcon.toHtml
                , Markdown.view { headingOffset = 1 } (gettext "Image is taken from OpenID profile or [Gravatar](https://gravatar.com)." appState.locale)
                ]
                :: timestampsView appState (UuidOrCurrent.isCurrent model.uuidOrCurrent) user
                ++ externalIdentitiesView appState user.externalIdentities
            )
        ]
    ]


{-| When the account was created and last changed, when its owner was last
online and accepted the consent — for an administrator looking at someone
else's account, not on one's own profile.
-}
timestampsView : AppState -> Bool -> User -> List (Html Msg)
timestampsView appState isCurrent user =
    if isCurrent then
        []

    else
        let
            dateTime =
                TimeUtils.toReadableDateTime appState.timeZone

            entry term testId value =
                DescriptionList.entry { term = term, description = value }
                    |> DescriptionList.withTestId testId

            optionalEntry term testId fallback value =
                entry term testId (Maybe.withDefault fallback (Maybe.map dateTime value))

            created =
                case user.createdAt of
                    Just createdAt ->
                        [ entry (gettext "Created" appState.locale) "user_created" (dateTime createdAt) ]

                    Nothing ->
                        []
        in
        [ DescriptionList.toHtml
            (created
                ++ [ entry (gettext "Updated" appState.locale) "user_updated" (dateTime user.updatedAt)
                   , optionalEntry (gettext "Last online" appState.locale) "user_last-online" (gettext "Never" appState.locale) user.lastVisitedAt
                   , optionalEntry (gettext "Consent" appState.locale) "user_consent" (gettext "Not accepted" appState.locale) user.consentAcceptedAt
                   ]
            )
        ]


{-| The accounts the user has connected, each as its provider's badge and the
account's identifier; nothing when there are none.
-}
externalIdentitiesView : AppState -> List UserIdentity -> List (Html Msg)
externalIdentitiesView appState identities =
    if List.isEmpty identities then
        []

    else
        let
            identityView identity =
                Layout.inline
                    [ ExternalLoginButton.badge { name = identity.providerName, style = identity.providerStyle }
                    , Html.text (UserIdentity.visibleIdentifier identity)
                    ]
        in
        [ Layout.stack
            (Text.label (gettext "Connected Accounts" appState.locale)
                :: List.map identityView (List.sortWith UserIdentity.compare identities)
            )
        ]


userFormView : AppState -> Model -> List Role -> Bool -> Html Msg
userFormView appState model roles isCurrent =
    let
        form =
            model.userForm

        adminFields =
            if isCurrent then
                []

            else
                [ Field.select appState form "role" (gettext "Role" appState.locale) (Role.toFormOptions appState.locale roles)
                , Field.toggle appState form "active" (gettext "Active" appState.locale)
                ]

        fields =
            [ Field.email appState form "email" (gettext "Email" appState.locale)
            , Field.input appState form "firstName" (gettext "First name" appState.locale)
            , Field.input appState form "lastName" (gettext "Last name" appState.locale)
            , Field.input appState form "affiliation" (gettext "Affiliation" appState.locale)
                |> Field.withSuggestions appState.config.users.affiliations
            ]
                ++ adminFields
    in
    CommonForm.view
        { onSubmit = EditFormMsg Form.Submit
        , content =
            -- Saving one's own profile reloads the application: the check on
            -- the bar's Save is the answer then, not a message gone in a moment.
            (if ActionResult.unwrap False (isCurrentUser appState) model.user then
                Html.text ""

             else
                successAlert model.savingUser
            )
                :: CommonForm.errorSummary model.savingUser
                :: List.map (Html.map EditFormMsg << Field.toHtml) fields
        }


{-| The unsaved-changes bar, in the settings template's footer slot: editing
what is already there has no natural moment to press Save, so the bar is the
one that appears when there is something to save.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    if ActionResult.isSuccess model.user then
        Just
            (CommonForm.unsavedBar appState
                { changed = FormUtils.containsChanges model.userForm
                , result = model.savingUser
                , onSave = EditFormMsg Form.Submit
                , onDiscard = Just Discard
                }
            )

    else
        Nothing


isCurrentUser : AppState -> User -> Bool
isCurrentUser appState user =
    Just user.uuid == Maybe.map .uuid appState.config.user


successAlert : ActionResult String -> Html msg
successAlert result =
    case result of
        Success message ->
            Alert.toHtml (Alert.success message)

        _ ->
            Html.text ""
