module Wizard.Pages.Dashboard.Widgets.ConfigureOrganizationWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


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
