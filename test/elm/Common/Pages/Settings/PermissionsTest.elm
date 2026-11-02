module Common.Pages.Settings.PermissionsTest exposing (suite)

{-| The shared role editor, where the general settings are not told from the
rest: one switch gives and takes both settings permissions.
-}

import Common.Data.WizardRolePermission as WizardRolePermission
import Common.Pages.Settings.Components.Permissions as Permissions
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Common.Pages.Settings.Components.Permissions"
        [ test "the settings switch gives both settings permissions" <|
            \_ ->
                Permissions.add WizardRolePermission.allGroups WizardRolePermission.settingsManage []
                    |> Expect.equal [ WizardRolePermission.settingsManage, WizardRolePermission.workspaceSettingsManage ]
        , test "and takes both" <|
            \_ ->
                Permissions.remove WizardRolePermission.allGroups WizardRolePermission.settingsManage [ WizardRolePermission.projectsView, WizardRolePermission.settingsManage, WizardRolePermission.workspaceSettingsManage ]
                    |> Expect.equal [ WizardRolePermission.projectsView ]
        , test "and the general settings have no switch of their own" <|
            \_ ->
                WizardRolePermission.allGroups
                    |> List.concatMap .permissions
                    |> List.map .permission
                    |> List.member WizardRolePermission.workspaceSettingsManage
                    |> Expect.equal False
        ]
