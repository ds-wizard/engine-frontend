module Wizard.Dashboard.Dashboards.WelcomeDashboard exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Locale exposing (lf)
import Shared.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Dashboard.Dashboards.WelcomeDashboard"


view : AppState -> Html msg
view appState =
    div [ class "WelcomeDashboard", dataCy "dashboard_welcome" ]
        [ Undraw.teaching
        , div [ class "fs-3" ]
            [ text <| lf_ "welcome" [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ] appState
            ]
        ]
