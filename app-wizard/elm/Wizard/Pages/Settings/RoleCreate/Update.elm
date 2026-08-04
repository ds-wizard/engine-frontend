module Wizard.Pages.Settings.RoleCreate.Update exposing (UpdateConfig, fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Api.Models.RolePermission as RolePermission
import Common.Data.WizardRolePermission as RolePermission
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.RequestHelpers as RequestHelpers
import Flip exposing (flip)
import Form
import Gettext exposing (gettext)
import List.Extra as List
import Wizard.Api.Roles as RolesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.RoleForm as RoleForm
import Wizard.Pages.Settings.RoleCreate.Models exposing (Model)
import Wizard.Pages.Settings.RoleCreate.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routes


fetchData : Cmd Msg
fetchData =
    Dom.focus "#name"


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        newRole =
                            RoleForm.encode model.permissions form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                RolesApi.postRole appState newRole PostRoleComplete
                    in
                    ( model, cmd )

                _ ->
                    let
                        form =
                            Form.update RoleForm.validation formMsg model.form
                    in
                    ( { model | form = form }, Cmd.none )

        AddPermission permission ->
            let
                impliedPermissions =
                    RolePermission.addImpliedPermissions permission RolePermission.all

                newPermissions =
                    (model.permissions ++ impliedPermissions)
                        |> List.unique
                        |> List.sortBy RolePermission.toString
            in
            ( { model | permissions = newPermissions }, Cmd.none )

        RemovePermission permission ->
            let
                impliedPermissions =
                    RolePermission.removeImpliedPermissions permission RolePermission.all

                newPermissions =
                    List.filter (not << flip List.member impliedPermissions) model.permissions
            in
            ( { model | permissions = newPermissions }, Cmd.none )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl Routes.settingsRoles) )

        PostRoleComplete result ->
            let
                ( newResult, cmd ) =
                    case result of
                        Ok _ ->
                            ( ActionResult.Success ()
                            , Routes.cmdNavigate appState Routes.settingsRoles
                            )

                        Err error ->
                            ( ApiError.toActionResult appState (gettext "Role could not be saved." appState.locale) error
                            , Cmd.batch
                                [ RequestHelpers.getResultCmd cfg.logoutMsg result
                                , Dom.scrollToTop ".container"
                                ]
                            )
            in
            ( { model | savingForm = newResult }, cmd )
