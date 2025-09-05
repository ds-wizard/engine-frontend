module Wizard.Pages.Dashboard.Dashboards.WelcomeDashboard exposing (view)

import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Data.AppState exposing (AppState)


view : AppState -> Html msg
view appState =
    div [ class "WelcomeDashboard", dataCy "dashboard_welcome" ]
        [ Undraw.teaching
        , div [ class "fs-3" ]
            [ text <| String.format (gettext "Welcome to the %s!" appState.locale) [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ]
            ]
        ]
