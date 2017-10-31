module Models exposing (..)

import Auth.Models
import Routing exposing (Route)


type alias Model =
    { route : Route
    , authModel : Auth.Models.Model
    }


initialModel : Route -> String -> Model
initialModel route token =
    { route = route
    , authModel = Auth.Models.initialModel token
    }


userLoggedIn : Model -> Bool
userLoggedIn model =
    model.authModel.token /= ""
