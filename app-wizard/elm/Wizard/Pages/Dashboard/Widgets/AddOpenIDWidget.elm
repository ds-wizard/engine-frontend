module Wizard.Pages.Dashboard.Widgets.AddOpenIDWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Add OpenID Service" appState.locale
        , text = gettext "OpenID is an open standard for authentication that allows using existing accounts to login to other services without creating a new password. Configure your organization's OpenID to make it easier for your users to log in." appState.locale
        , action =
            { route = Routes.settingsAuthentication
            , label = gettext "Add OpenID" appState.locale
            }
        }
