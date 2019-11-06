module Wizard.Dashboard.Widgets.WelcomeWidget exposing (view)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lf)


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Dashboard.Widgets.WelcomeWidget"


view : AppState -> Html msg
view appState =
    div [ class "WelcomeWidget" ]
        [ img [ src "/img/illustrations/undraw_teaching.svg" ] []
        , div [ class "WelcomeWidget__Message" ]
            [ text <| lf_ "welcome" [ appState.config.client.appTitle ] appState
            ]
        ]
