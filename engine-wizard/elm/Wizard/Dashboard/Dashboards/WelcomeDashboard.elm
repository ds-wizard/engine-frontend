module Wizard.Dashboard.Dashboards.WelcomeDashboard exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Undraw as Undraw
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


view : AppState -> Html msg
view appState =
    div [ class "WelcomeDashboard", dataCy "dashboard_welcome" ]
        [ Undraw.teaching
        , div [ class "fs-3" ]
            [ text <| String.format (gettext "Welcome to the %s!" appState.locale) [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ]
            ]
        ]
