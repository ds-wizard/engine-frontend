module Public.Models exposing (..)

import Public.Login.Models
import Public.Routing exposing (Route(..))


type alias Model =
    { loginModel : Public.Login.Models.Model
    }


initialModel : Model
initialModel =
    { loginModel = Public.Login.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Login ->
            { model | loginModel = Public.Login.Models.initialModel }

        _ ->
            model
