module Wizard.Public.Login.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { email : String
    , password : String
    , loggingIn : ActionResult String
    , originalUrl : Maybe String
    }


initialModel : Maybe String -> Model
initialModel mbOriginalUrl =
    { email = ""
    , password = ""
    , loggingIn = Unset
    , originalUrl = mbOriginalUrl
    }
