module Wizard.Dashboard.Widgets.ConnectRegistryWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Connect to DSW Registry" appState.locale
        , text = gettext "[DSW Registry](https://registry.ds-wizard.org) is a place for published knowledge models and document templates. When you connect your instance, data stewards can easily import them." appState.locale
        , action =
            { route = Routes.settingsRegistry
            , label = gettext "Connect" appState.locale
            }
        }
