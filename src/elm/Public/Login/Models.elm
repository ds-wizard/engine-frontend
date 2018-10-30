module Public.Login.Models exposing (..)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { email : String
    , password : String
    , loggingIn : ActionResult String
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , loggingIn = Unset
    }
