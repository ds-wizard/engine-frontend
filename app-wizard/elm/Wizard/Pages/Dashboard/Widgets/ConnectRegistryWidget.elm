module Wizard.Pages.Dashboard.Widgets.ConnectRegistryWidget exposing (enabled, view)

import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.settingsManage appState
        && not (RegistryConfig.isEnabled appState.config.registry)


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Connect to DSW Registry" appState.locale
        , text = gettext "[DSW Registry](https://registry.ds-wizard.org) is a place for published knowledge models and document templates. When you connect your instance, data stewards can easily import them." appState.locale
        , action =
            { route = Routes.settingsRegistry
            , label = gettext "Connect" appState.locale
            }
        }
