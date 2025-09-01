module Wizard.Public.Auth.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Shared.Data.Token exposing (Token)


type alias Model =
    { id : String
    , sessionState : Maybe String
    , consent : Bool
    , authenticating : ActionResult String
    , hash : Maybe String
    , submittingConsent : ActionResult String
    , originalUrl : ActionResult (Maybe String)
    , token : ActionResult Token
    }


initialModel : String -> Maybe String -> Model
initialModel id sessionState =
    { id = id
    , sessionState = sessionState
    , consent = False
    , authenticating = ActionResult.Loading
    , hash = Nothing
    , submittingConsent = ActionResult.Unset
    , originalUrl = ActionResult.Loading
    , token = ActionResult.Loading
    }
