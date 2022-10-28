module Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    let
        widgetText =
            if appState.config.feature.clientCustomizationEnabled then
                gettext "You can configure the application name and easily change the logo and colors to match your organization's style. You can also add additional menu links, for example, to your guidelines or documentation." appState.locale

            else
                gettext "You can configure the application name, or add additional menu links, for example, to your guidelines or documentation." appState.locale
    in
    WidgetHelpers.ctaWidget appState
        { title = gettext "Configure Look & Feel" appState.locale
        , text = widgetText
        , action =
            { route = Routes.settingsLookAndFeel
            , label = gettext "Configure" appState.locale
            }
        }
