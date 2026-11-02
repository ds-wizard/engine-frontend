module Common.Pages.Settings.RoleDetail exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Models.RolePermission exposing (PermissionGroup, RolePermission)
import Common.Api.Roles as RolesApi
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.RoleForm as RoleForm exposing (RoleForm)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Permissions as Permissions
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Layout as Layout
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , role : ActionResult Role
    , form : Form FormError RoleForm
    , permissions : List RolePermission
    , savingRole : ActionResult ()
    }


type Msg
    = GetRoleComplete (Result ApiError Role)
    | PutRoleComplete (Result ApiError ())
    | FormMsg Form.Msg
    | SetPermissions (List RolePermission)
    | Discard
    | FocusDone


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , role = Loading
    , form = RoleForm.initEmpty
    , permissions = []
    , savingRole = Unset
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel uuid, RolesApi.getRole appState uuid GetRoleComplete )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetRoleComplete result ->
            case result of
                Ok role ->
                    ( { model | role = Success role, form = RoleForm.init role, permissions = role.permissions }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.role appState)
                        )

                    else
                        ( { model | role = ApiError.toActionResult appState (gettext "Unable to get role" appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        PutRoleComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingRole = Success () }, CommonForm.reloadWhenSaved )

                Err error ->
                    ( { model | savingRole = ApiError.toActionResult appState (gettext "Role could not be saved." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    ( { model | savingRole = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        RolesApi.putRole appState model.uuid (RoleForm.encode model.permissions form) PutRoleComplete
                    )

                _ ->
                    ( { model | form = Form.update RoleForm.validation formMsg model.form }
                    , Cmd.none
                    )

        SetPermissions permissions ->
            ( { model | permissions = permissions }, Cmd.none )

        Discard ->
            case model.role of
                Success role ->
                    ( { model | form = RoleForm.init role, permissions = role.permissions, savingRole = Unset }
                    , Cmd.map cfg.wrapMsg (Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId))
                    )

                _ ->
                    ( model, Cmd.none )

        FocusDone ->
            ( model, Cmd.none )


formChanged : Model -> Bool
formChanged model =
    case model.role of
        Success role ->
            not role.isAdmin && (model.permissions /= role.permissions || FormUtils.containsChanges model.form)

        _ ->
            False


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if formChanged model then
        CommonForm.submitShortcut appState { result = model.savingRole, onSubmit = FormMsg Form.Submit }

    else
        Sub.none


view : { groups : List PermissionGroup, note : Maybe String, reach : Maybe String } -> AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view permissions appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Edit Role" appState.locale) []
            , Section.actionResult appState (viewRole permissions appState model) model.role
            ]
        ]
    , footer =
        if ActionResult.isSuccess model.role then
            Just
                -- A successful save reloads the application: the bar goes
                -- first, showing the check on its Save.
                (CommonForm.unsavedBar appState
                    { changed = formChanged model && not (ActionResult.isSuccess model.savingRole)
                    , result = model.savingRole
                    , onSave = FormMsg Form.Submit
                    , onDiscard = Just Discard
                    }
                )

        else
            Nothing
    }


viewRole : { groups : List PermissionGroup, note : Maybe String, reach : Maybe String } -> AppState -> Model -> Role -> Html Msg
viewRole permissions appState model role =
    let
        adminRoleInfo =
            if role.isAdmin then
                Alert.toHtml (Alert.info (gettext "Admin role has all permissions and cannot be modified." appState.locale))

            else
                Html.text ""

        -- A disabled input rather than text: the same field as on every other
        -- role, so the admin role reads as this form with nothing editable, as
        -- its permissions below do.
        nameField =
            Field.input appState model.form "name" (gettext "Name" appState.locale)
                |> Field.withDisabled role.isAdmin
    in
    Layout.stack
        [ adminRoleInfo
        , CommonForm.errorSummary model.savingRole
        , CommonForm.view
            { onSubmit = FormMsg Form.Submit
            , content =
                [ Html.map FormMsg (Field.toHtml nameField)
                , Permissions.view appState
                    { groups = permissions.groups
                    , note = permissions.note
                    , reach = permissions.reach
                    , selected = model.permissions
                    , onChange = SetPermissions
                    , disabled = role.isAdmin
                    }
                ]
            }
        ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (formChanged model && not (ActionResult.isSuccess model.savingRole))


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Gettext.Locale -> Model -> Maybe String
entityName locale model =
    Maybe.map (Role.localizedName locale) (ActionResult.toMaybe model.role)
