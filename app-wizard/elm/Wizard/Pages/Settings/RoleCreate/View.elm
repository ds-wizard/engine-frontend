module Wizard.Pages.Settings.RoleCreate.View exposing (view)

import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Components.SettingsPermissionGroup as SettingsPermissionGroup
import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.RoleCreate.Models exposing (Model)
import Wizard.Pages.Settings.RoleCreate.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        permissionGroupProps group =
            { group = group
            , locale = appState.locale
            , selectedPermissions = model.permissions
            , removePermissionMsg = RemovePermission
            , addPermissionMsg = AddPermission
            , disabled = False
            }

        permissionGroups =
            List.map (SettingsPermissionGroup.view << permissionGroupProps) RolePermission.allGroups

        formContent =
            div []
                [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
                , div [] permissionGroups
                ]
    in
    div [ class "pb-6" ]
        [ Page.header (gettext "Create Role" appState.locale) []
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingForm
            , formView = formContent
            , submitLabel = gettext "Create" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
        ]
