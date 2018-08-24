module Public.SignupConfirmation.Models exposing (..)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { confirmation : ActionResult String
    }


initialModel : Model
initialModel =
    { confirmation = Loading
    }
