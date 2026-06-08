module Wizard.Pages.Dashboard.Widgets.ImportKnowledgeModelWidget exposing (enabled, view)

import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.knowledgeModelsManage appState


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Import Knowledge Model" appState.locale
        , text =
            String.format
                (gettext "Knowledge models are published in [%s](%s). You can easily import them into your instance to make them available for researchers. You can also import other knowledge models exported from different instances." appState.locale)
                [ LookAndFeelConfig.defaultRegistryName, LookAndFeelConfig.defaultRegistryUrl ++ "/knowledge-models" ]
        , action =
            { route = Routes.knowledgeModelsImport Nothing
            , label = gettext "Import" appState.locale
            }
        }
