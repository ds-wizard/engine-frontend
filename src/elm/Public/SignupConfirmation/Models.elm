module Public.SignupConfirmation.Models exposing (..)

import Common.Types exposing (ActionResult(..))


type alias Model =
    { confirmation : ActionResult String
    }


initialModel : Model
initialModel =
    { confirmation = Loading
    }
