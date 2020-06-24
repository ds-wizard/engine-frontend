module Wizard.Dashboard.Widgets.WelcomeWidget exposing (view)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Locale exposing (lf)
import Wizard.Common.AppState exposing (AppState)


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Dashboard.Widgets.WelcomeWidget"


view : AppState -> Html msg
view appState =
    div [ class "WelcomeWidget" ]
        [ img [ src "/img/illustrations/undraw_teaching.svg" ] []
        , div [ class "WelcomeWidget__Message" ]
            [ text <| lf_ "welcome" [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ] appState
            ]
        ]
