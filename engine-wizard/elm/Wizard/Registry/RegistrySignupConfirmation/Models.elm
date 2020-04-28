module Wizard.Registry.RegistrySignupConfirmation.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))


type alias Model =
    { confirmation : ActionResult ()
    }


initialModel : Model
initialModel =
    { confirmation = Loading
    }
