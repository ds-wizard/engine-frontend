module Wizard.Pages.Dashboard.Widgets.ConfigureLookAndFeelWidget exposing (enabled, view)

import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.settingsManage appState
        && not (Admin.isEnabled appState.config.admin)
        && (Maybe.isNothing appState.config.lookAndFeel.appTitle || Maybe.isNothing appState.config.lookAndFeel.appTitleShort)


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Configure Look & Feel" appState.locale
        , text = gettext "You can configure the application name, or add additional menu links, for example, to your guidelines or documentation." appState.locale
        , action =
            { route = Routes.settingsLookAndFeel
            , label = gettext "Configure" appState.locale
            }
        }
