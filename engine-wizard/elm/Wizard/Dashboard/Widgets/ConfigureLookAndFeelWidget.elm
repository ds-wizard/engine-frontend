module Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Configure Look & Feel" appState.locale
        , text = gettext "You can configure the application name, or add additional menu links, for example, to your guidelines or documentation." appState.locale
        , action =
            { route = Routes.settingsLookAndFeel
            , label = gettext "Configure" appState.locale
            }
        }
