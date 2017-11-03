module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionExists)
import Routing exposing (Route)
import UserManagement.Index.Models


type alias Model =
    { route : Route
    , authModel : AuthModels.Model
    , session : Session
    , jwt : Maybe JwtToken
    , userManagementIndexModel : UserManagement.Index.Models.Model
    }


initialModel : Route -> Session -> Maybe JwtToken -> Model
initialModel route session jwt =
    { route = route
    , authModel = AuthModels.initialModel
    , session = session
    , jwt = jwt
    , userManagementIndexModel = UserManagement.Index.Models.initialModel
    }


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.session
