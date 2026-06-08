module Wizard.Pages.Dashboard.Widgets.ConfigureOrganizationWidget exposing (enabled, view)

import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.settingsManage appState
        && (appState.config.organization.name == "My Organization" || appState.config.organization.organizationId == "myorg")


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Configure Organization Info" appState.locale
        , text = gettext "Fill in your organization name and organization ID. For example, these values will be used with knowledge models created in this instance." appState.locale
        , action =
            { route = Routes.settingsOrganization
            , label = gettext "Configure" appState.locale
            }
        }
