module Common.Pages.Settings.RoleCreate exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Role exposing (Role)
import Common.Api.Models.RolePermission exposing (PermissionGroup, RolePermission)
import Common.Api.Roles as RolesApi
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.RoleForm as RoleForm exposing (RoleForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Permissions as Permissions
import Common.Pages.Settings.Components.Section as Section
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Layout as Layout


type alias Model =
    { form : Form FormError RoleForm
    , permissions : List RolePermission
    , savingForm : ActionResult ()
    }


type Msg
    = FormMsg Form.Msg
    | SetPermissions (List RolePermission)
    | Cancel
    | PostRoleComplete (Result ApiError Role)


initialModel : Model
initialModel =
    { form = RoleForm.initEmpty
    , permissions = []
    , savingForm = Unset
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingForm = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        RolesApi.postRole appState (RoleForm.encode model.permissions form) PostRoleComplete
                    )

                _ ->
                    ( { model | form = Form.update RoleForm.validation formMsg model.form }
                    , Cmd.none
                    )

        SetPermissions permissions ->
            ( { model | permissions = permissions }, Cmd.none )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.settingsRoles) )

        PostRoleComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingForm = Success () }
                    , Routes.navigate appState Routes.settingsRoles
                    )

                Err error ->
                    ( { model | savingForm = ApiError.toActionResult appState (gettext "Role could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    CommonForm.submitShortcut appState { result = model.savingForm, onSubmit = FormMsg Form.Submit }


view : { groups : List PermissionGroup, note : Maybe String, reach : Maybe String } -> AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view permissions appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Create Role" appState.locale) []
            , CommonForm.createView
                { onSubmit = FormMsg Form.Submit
                , content =
                    [ CommonForm.errorSummary model.savingForm
                    , Html.map FormMsg <|
                        Field.toHtml (Field.input appState model.form "name" (gettext "Name" appState.locale))
                    , Permissions.view appState
                        { groups = permissions.groups
                        , note = permissions.note
                        , reach = permissions.reach
                        , selected = model.permissions
                        , onChange = SetPermissions
                        , disabled = False
                        }
                    , CommonForm.actions appState
                        { submitLabel = gettext "Create" appState.locale
                        , result = model.savingForm
                        , onCancel = Just Cancel
                        }
                    ]
                }
            ]
        ]
    , footer = Nothing
    }
