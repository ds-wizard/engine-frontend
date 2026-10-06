module Wizard.Pages.Settings.RoleDetail.Update exposing (UpdateConfig, fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Api.Models.RolePermission as RolePermission
import Common.Data.WizardRolePermission as RolePermission
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.RequestHelpers as RequestHelpers
import Flip exposing (flip)
import Form
import Gettext exposing (gettext)
import List.Extra as List
import Uuid exposing (Uuid)
import Wizard.Api.Roles as RolesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.RoleForm as RoleForm
import Wizard.Pages.Settings.RoleDetail.Models exposing (Model)
import Wizard.Pages.Settings.RoleDetail.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    RolesApi.getRole appState uuid GetRoleComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetRoleComplete result ->
            case result of
                Ok role ->
                    ( { model
                        | role = ActionResult.Success role
                        , form = RoleForm.init role
                        , permissions = role.permissions
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | role = ApiError.toActionResult appState (gettext "Unable to get role" appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PutRoleComplete result ->
            let
                ( newResult, cmd ) =
                    case result of
                        Ok _ ->
                            ( ActionResult.Success ()
                            , Window.refresh ()
                            )

                        Err error ->
                            ( ApiError.toActionResult appState (gettext "Role could not be saved." appState.locale) error
                            , RequestHelpers.getResultCmd cfg.logoutMsg result
                            )
            in
            ( { model | savingRole = newResult }
            , Cmd.batch [ cmd, Dom.scrollToTop ".Settings__content" ]
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        newRole =
                            RoleForm.encode model.permissions form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                RolesApi.putRole appState model.uuid newRole PutRoleComplete
                    in
                    ( model, cmd )

                _ ->
                    let
                        form =
                            Form.update RoleForm.validation formMsg model.form
                    in
                    ( { model | form = form }
                    , FormUtils.scrollToInvalidField formMsg
                    )

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
