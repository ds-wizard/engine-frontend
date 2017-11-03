module Models exposing (..)

import Auth.Models as AuthModels exposing (Session, sessionExists)
import Routing exposing (Route)


type alias Model =
    { route : Route
    , authModel : AuthModels.Model
    , session : Session
    }


initialModel : Route -> Session -> Model
initialModel route session =
    { route = route
    , authModel = AuthModels.initialModel
    , session = session
    }


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.session
