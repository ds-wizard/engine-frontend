module Dashboard.Widgets.WelcomeWidget exposing (view)

import Common.AppState exposing (AppState)
import Common.Locale exposing (lf)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Dashboard.Widgets.WelcomeWidget"


view : AppState -> Html msg
view appState =
    div [ class "WelcomeWidget" ]
        [ img [ src "/img/illustrations/undraw_teaching.svg" ] []
        , div [ class "WelcomeWidget__Message" ]
            [ text <| lf_ "welcome" [ appState.config.client.appTitle ] appState
            ]
        ]
