module Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget exposing (view)

import Html exposing (Html)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget"


view : AppState -> Html msg
view appState =
    let
        widgetText =
            if appState.config.feature.clientCustomizationEnabled then
                l_ "textWithCustomizations" appState

            else
                l_ "textWithoutCustomizations" appState
    in
    WidgetHelpers.ctaWidget appState
        { title = l_ "title" appState
        , text = widgetText
        , action =
            { route = Routes.settingsLookAndFeel
            , label = l_ "actionLabel" appState
            }
        }
