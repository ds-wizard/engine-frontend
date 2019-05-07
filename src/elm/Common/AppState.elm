module Common.AppState exposing
    ( AppState
    , getDashboardWidgets
    )

import Auth.Models exposing (JwtToken, Session)
import Browser.Navigation exposing (Key)
import Common.Config exposing (Config, Widget(..))
import Dict
import Random exposing (Seed)
import Routing exposing (Route(..))


type alias AppState =
    { route : Route
    , seed : Seed
    , session : Session
    , jwt : Maybe JwtToken
    , key : Key
    , apiUrl : String
    , config : Config
    , valid : Bool
    }


getDashboardWidgets : AppState -> List Widget
getDashboardWidgets appState =
    let
        role =
            appState.session.user
                |> Maybe.map .role
                |> Maybe.withDefault ""
    in
    Dict.get role appState.config.client.dashboard
        |> Maybe.withDefault [ Welcome ]
