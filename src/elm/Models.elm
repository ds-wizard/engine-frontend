module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionExists)
import Routing exposing (Route)
import UserManagement.Create.Models
import UserManagement.Delete.Models
import UserManagement.Index.Models


type alias Model =
    { route : Route
    , authModel : AuthModels.Model
    , session : Session
    , jwt : Maybe JwtToken
    , userManagementIndexModel : UserManagement.Index.Models.Model
    , userManagementCreateModel : UserManagement.Create.Models.Model
    , userManagementDeleteModel : UserManagement.Delete.Models.Model
    }


initialModel : Route -> Session -> Maybe JwtToken -> Model
initialModel route session jwt =
    { route = route
    , authModel = AuthModels.initialModel
    , session = session
    , jwt = jwt
    , userManagementIndexModel = UserManagement.Index.Models.initialModel
    , userManagementCreateModel = UserManagement.Create.Models.initialModel 0
    , userManagementDeleteModel = UserManagement.Delete.Models.initialModel
    }


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.session
