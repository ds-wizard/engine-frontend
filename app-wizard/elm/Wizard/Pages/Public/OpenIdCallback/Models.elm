module Wizard.Pages.Public.OpenIdCallback.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.Token exposing (Token)
import Common.Components.UserExternalCompletionForm as UserExternalCompletionForm


type alias Model =
    { id : String
    , sessionState : Maybe String
    , consent : Bool
    , authenticating : ActionResult String
    , hash : Maybe String
    , submittingConsent : ActionResult String
    , originalUrl : ActionResult (Maybe String)
    , token : ActionResult Token
    , completionForm : Maybe UserExternalCompletionForm.Model
    , completingRegistration : ActionResult String
    , emailVerificationRequired : Bool
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
    , completionForm = Nothing
    , completingRegistration = ActionResult.Unset
    , emailVerificationRequired = False
    }
