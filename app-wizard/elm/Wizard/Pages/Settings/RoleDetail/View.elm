module Wizard.Pages.Settings.RoleDetail.View exposing (view)

import Common.Api.Models.Role exposing (Role)
import Common.Components.Flash as Flash
import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Components.SettingsPermissionGroup as SettingsPermissionGroup
import Common.Data.WizardRolePermission as RolePermission
import Common.Utils.Form as Form
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class, readonly)
import Html.Extra as Html
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.RoleDetail.Models exposing (Model)
import Wizard.Pages.Settings.RoleDetail.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewRole appState model) model.role


viewRole : AppState -> Model -> Role -> Html Msg
viewRole appState model role =
    let
        permissionGroupProps group =
            { group = group
            , locale = appState.locale
            , selectedPermissions = model.permissions
            , removePermissionMsg = RemovePermission
            , addPermissionMsg = AddPermission
            , disabled = role.isAdmin
            }

        permissionGroups =
            List.map (SettingsPermissionGroup.view << permissionGroupProps) RolePermission.allGroups

        formContent =
            div []
                [ Html.map FormMsg <| FormGroup.inputAttrs [ readonly role.isAdmin ] appState.locale model.form "name" (gettext "Name" appState.locale)
                , div [] permissionGroups
                ]

        permissionsChanged =
            model.permissions /= role.permissions

        adminRoleInfo =
            Html.viewIf role.isAdmin <|
                Flash.info (gettext "Admin role has all permissions and cannot be modified." appState.locale)

        formChanged =
            not role.isAdmin && (permissionsChanged || Form.containsChanges model.form)

        form =
            Form.initDynamic appState (FormMsg Form.Submit) model.savingRole
                |> Form.setFormView formContent
                |> Form.setFormChanged formChanged
                |> Form.setWide
                |> Form.viewDynamic
    in
    div [ class "pb-6" ]
        [ Page.header (gettext "Edit Role" appState.locale) []
        , adminRoleInfo
        , form
        ]
