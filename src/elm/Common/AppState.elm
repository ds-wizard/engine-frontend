module Common.AppState exposing (AppState)

import Auth.Models exposing (JwtToken, Session)
import Browser.Navigation exposing (Key)
import Random exposing (Seed)
import Routing exposing (Route(..))


type alias AppState =
    { route : Route
    , seed : Seed
    , session : Session
    , jwt : Maybe JwtToken
    , key : Key
    , apiUrl : String
    , appTitle : String
    , appTitleShort : String
    , welcome :
        { warning : Maybe String
        , info : Maybe String
        }
    }
