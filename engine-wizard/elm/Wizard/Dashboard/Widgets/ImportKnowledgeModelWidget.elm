module Wizard.Dashboard.Widgets.ImportKnowledgeModelWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
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
