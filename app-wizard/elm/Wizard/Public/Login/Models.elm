module Wizard.Public.Login.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { email : String
    , password : String
    , code : String
    , loggingIn : ActionResult String
    , originalUrl : Maybe String
    , codeRequired : Bool
    }


initialModel : Maybe String -> Model
initialModel mbOriginalUrl =
    { email = ""
    , password = ""
    , code = ""
    , loggingIn = Unset
    , originalUrl = mbOriginalUrl
    , codeRequired = False
    }
