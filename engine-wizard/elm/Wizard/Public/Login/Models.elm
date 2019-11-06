module Wizard.Public.Login.Models exposing (Model, initialModel)

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
