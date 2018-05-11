module Public.Login.Models exposing (..)

import Common.Types exposing (ActionResult(..))


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
