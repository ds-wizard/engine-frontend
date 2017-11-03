module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionExists)
import Routing exposing (Route)


type alias Model =
    { route : Route
    , authModel : AuthModels.Model
    , session : Session
    , jwt : Maybe JwtToken
    }


initialModel : Route -> Session -> Maybe JwtToken -> Model
initialModel route session jwt =
    { route = route
    , authModel = AuthModels.initialModel
    , session = session
    , jwt = jwt
    }


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.session
