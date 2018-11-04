module Public.SignupConfirmation.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { confirmation : ActionResult String
    }


initialModel : Model
initialModel =
    { confirmation = Loading
    }
