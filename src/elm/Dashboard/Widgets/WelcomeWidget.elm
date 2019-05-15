module Dashboard.Widgets.WelcomeWidget exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)


view : AppState -> Html msg
view appState =
    div [ class "WelcomeWidget" ]
        [ img [ src "/img/illustrations/undraw_teaching.svg" ] []
        , div [ class " WelcomeWidget__Message" ]
            [ text <| "Welcome to the " ++ appState.config.client.appTitle ++ "!"
            ]
        ]
