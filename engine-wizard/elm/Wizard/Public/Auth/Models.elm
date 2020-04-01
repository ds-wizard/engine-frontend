module Wizard.Public.Auth.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))


type alias Model =
    { authenticating : ActionResult String
    }


initialModel : Model
initialModel =
    { authenticating = Loading
    }
