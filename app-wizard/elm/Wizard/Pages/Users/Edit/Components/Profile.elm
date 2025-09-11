module Wizard.Pages.Users.Edit.Components.Profile exposing
    ( Model
    , Msg(..)
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (fa, faInfo)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Data.Role as Role
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Markdown as Markdown
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, img, strong, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onSubmit)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.User as User exposing (User)
import Wizard.Api.Users as UsersApi
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserEditForm as UserEditForm exposing (UserEditForm)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , user : ActionResult User
    , savingUser : ActionResult String
    , userForm : Form FormError UserEditForm
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , user = ActionResult.Loading
    , savingUser = ActionResult.Unset
    , userForm = UserEditForm.initEmpty
    }


type Msg
    = GetUserCompleted (Result ApiError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError User)


fetchData : AppState -> UuidOrCurrent -> Cmd Msg
fetchData appState uuidOrCurrent =
    UsersApi.getUser appState uuidOrCurrent GetUserCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        EditFormMsg formMsg ->
            handleUserForm cfg appState formMsg model

        GetUserCompleted result ->
            getUserCompleted cfg appState model result

        PutUserCompleted result ->
            putUserCompleted cfg appState model result


handleUserForm : UpdateConfig msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleUserForm cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                body =
                    UserEditForm.encode model.uuidOrCurrent userForm

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        UsersApi.putUser appState model.uuidOrCurrent body PutUserCompleted
            in
            ( { model | savingUser = ActionResult.Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update UserEditForm.validation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


getUserCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError User -> ( Model, Cmd msg )
getUserCompleted cfg appState model result =
    let
        newModel =
            case result of
                Ok user ->
                    let
                        userForm =
                            UserEditForm.init user
                    in
                    { model | userForm = userForm, user = ActionResult.Success user }

                Err error ->
                    { model | user = ApiError.toActionResult appState (gettext "Unable to get the user." appState.locale) error }

        cmd =
            RequestHelpers.getResultCmd cfg.logoutMsg result
    in
    ( newModel, cmd )


putUserCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError User -> ( Model, Cmd msg )
putUserCompleted cfg appState model result =
    case result of
        Ok user ->
            let
                updateCmd =
                    if Just user.uuid == Maybe.map .uuid appState.config.user then
                        Window.refresh ()

                    else
                        Cmd.none
            in
            ( { model | savingUser = ActionResult.Success <| gettext "Profile was successfully updated." appState.locale }
            , Cmd.batch
                [ Dom.scrollToTop ".Users__Edit__content"
                , updateCmd
                ]
            )

        Err err ->
            ( { model
                | savingUser = ApiError.toActionResult appState (gettext "Profile could not be saved." appState.locale) err
                , userForm = Form.setFormErrors appState err model.userForm
              }
            , Cmd.batch
                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                , Dom.scrollToTop ".Users__Edit__content"
                ]
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (userView appState model) model.user


userView : AppState -> Model -> User -> Html Msg
userView appState model user =
    let
        content =
            if Admin.isEnabled appState.config.admin then
                readOnlyView appState user

            else
                Html.map EditFormMsg <|
                    userFormView appState model user (UuidOrCurrent.isCurrent model.uuidOrCurrent)
    in
    div []
        [ Page.header (gettext "Profile" appState.locale) []
        , div [ class "row" ]
            [ content
            , div [ class "col-4" ]
                [ div [ class "col-border-left" ]
                    [ strong [] [ text (gettext "User Image" appState.locale) ]
                    , div []
                        [ img [ src (User.imageUrl user), class "user-icon user-icon-large" ] []
                        ]
                    , Markdown.toHtml [ class "text-muted" ] (gettext "Image is taken from OpenID profile or [Gravatar](https://gravatar.com)." appState.locale)
                    ]
                ]
            ]
        ]


userFormView : AppState -> Model -> User -> Bool -> Html Form.Msg
userFormView appState model user isCurrent =
    let
        roleSelect =
            if isCurrent then
                Html.nothing

            else
                FormGroup.select appState.locale (Role.options appState) model.userForm "role" <| gettext "Role" appState.locale

        activeToggle =
            if isCurrent then
                Html.nothing

            else
                FormGroup.toggle model.userForm "active" <| gettext "Active" appState.locale
    in
    Html.form [ onSubmit Form.Submit, class "col-8" ]
        [ FormResult.view model.savingUser
        , FormGroup.input appState.locale model.userForm "email" <| gettext "Email" appState.locale
        , FormExtra.blockAfter (List.map (ExternalLoginButton.badgeWrapper appState.locale appState.config) user.sources)
        , FormGroup.input appState.locale model.userForm "firstName" <| gettext "First name" appState.locale
        , FormGroup.input appState.locale model.userForm "lastName" <| gettext "Last name" appState.locale
        , FormGroup.inputWithTypehints appState.config.organization.affiliations appState.locale model.userForm "affiliation" <| gettext "Affiliation" appState.locale
        , roleSelect
        , activeToggle
        , div [ class "mt-5" ]
            [ ActionButton.submit (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingUser) ]
        ]


readOnlyView : AppState -> User -> Html msg
readOnlyView appState user =
    let
        editProfileUrl base =
            base ++ "/users/edit/current"

        readOnlyInfo =
            div [ class "alert alert-info" ]
                [ faInfo
                , text (gettext "Your profile is managed elsewhere." appState.locale)
                , a
                    [ class "btn btn-primary ms-2"
                    , href (Maybe.unwrap "" editProfileUrl (Admin.getClientUrl appState.config.admin))
                    ]
                    [ text (gettext "Edit profile" appState.locale)
                    , fa "fas fa-external-link-alt ms-2"
                    ]
                ]
    in
    div [ class "col-8" ]
        [ readOnlyInfo
        , FormGroup.readOnlyInput user.email (gettext "Email" appState.locale)
        , FormExtra.blockAfter (List.map (ExternalLoginButton.badgeWrapper appState.locale appState.config) user.sources)
        , FormGroup.readOnlyInput user.firstName (gettext "First name" appState.locale)
        , FormGroup.readOnlyInput user.lastName (gettext "Last name" appState.locale)
        , FormGroup.readOnlyInput (Maybe.withDefault "" user.affiliation) (gettext "Affiliation" appState.locale)
        ]
