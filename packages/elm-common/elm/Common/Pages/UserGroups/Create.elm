module Common.Pages.UserGroups.Create exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Create a user group in the workspace. Once it is there, the reader is
taken to it, to add its members.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.UserGroups as UserGroupsApi
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.UserGroupForm as UserGroupForm exposing (UserGroupForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.UserGroups.Routes as UserGroupsRoutes
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert
import Uuid exposing (Uuid)


type alias Model =
    { savingUserGroup : ActionResult String
    , form : Form FormError UserGroupForm
    }


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostUserGroupCompleted (Result ApiError Uuid)


initialModel : Model
initialModel =
    { savingUserGroup = Unset
    , form = UserGroupForm.initEmpty
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Cancel ->
            ( model
            , Window.historyBack (Routes.toUrl appState (Routes.UserGroupsRoute (UserGroupsRoutes.Index PaginationQueryString.empty Nothing)))
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingUserGroup = Loading }
                    , UserGroupsApi.postUserGroup appState (UserGroupForm.encode form) (cfg.wrapMsg << PostUserGroupCompleted)
                    )

                _ ->
                    ( { model | form = Form.update UserGroupForm.validation formMsg model.form }
                    , Cmd.none
                    )

        PostUserGroupCompleted result ->
            case result of
                Ok uuid ->
                    ( model
                    , Routes.navigate appState (Routes.userGroupsDetail uuid)
                    )

                Err error ->
                    ( { model
                        | savingUserGroup = ApiError.toActionResult appState (gettext "User group could not be created." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , FormLayout.focusErrorSummary
                        ]
                    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    FormLayout.submitShortcut appState { result = model.savingUserGroup, onSubmit = FormMsg Form.Submit }


view : AppState -> Model -> Html Msg
view appState model =
    FormLayout.createPage appState
        { title = gettext "Create User Group" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) guideLink ]
        , content = notMemberWarning appState ++ List.map (Html.map FormMsg) (formFields appState model.form)
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingUserGroup
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }


{-| Someone who is in the workspace through their organization role alone is
not made the group's owner, so they hear it before they make it.
-}
notMemberWarning : AppState -> List (Html msg)
notMemberWarning appState =
    if appState.workspaceMember then
        []

    else
        [ Alert.toHtml (Alert.warning (gettext "You are not a member of this workspace, so you will not be the owner of this group. It will be managed by those who may manage every user group here." appState.locale)) ]


formFields : AppState -> Form FormError UserGroupForm -> List (Html Form.Msg)
formFields appState form =
    [ Field.input appState form "name" (gettext "Name" appState.locale)
        |> Field.withRequired True
    , Field.input appState form "description" (gettext "Description" appState.locale)
    , Field.toggle appState form "private" (gettext "Private" appState.locale)
        |> Field.withHint (gettext "If enabled, this group is only visible to other members of the group and instance admins." appState.locale)
    ]
        |> List.map Field.toHtml


guideLink : GuideLinks -> String
guideLink =
    GuideLinks.userGroupsCreate
