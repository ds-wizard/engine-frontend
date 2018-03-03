module Public.Models exposing (..)

import Public.Login.Models
import Public.Routing exposing (Route(..))
import Public.Signup.Models


type alias Model =
    { loginModel : Public.Login.Models.Model
    , signupModel : Public.Signup.Models.Model
    }


initialModel : Model
initialModel =
    { loginModel = Public.Login.Models.initialModel
    , signupModel = Public.Signup.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Login ->
            { model | loginModel = Public.Login.Models.initialModel }

        Signup ->
            { model | signupModel = Public.Signup.Models.initialModel }

        _ ->
            model
