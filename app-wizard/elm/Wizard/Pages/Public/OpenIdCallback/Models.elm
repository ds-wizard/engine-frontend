module Wizard.Pages.Public.OpenIdCallback.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.Token exposing (Token)
import Common.Components.UserExternalCompletionForm as UserExternalCompletionForm


type alias Model =
    { id : String
    , error : Maybe String
    , code : Maybe String
    , sessionState : Maybe String
    , state : Maybe String
    , consent : Bool
    , authenticating : ActionResult String
    , hash : Maybe String
    , submittingConsent : ActionResult String
    , originalUrl : ActionResult (Maybe String)
    , originalState : ActionResult (Maybe String)
    , token : ActionResult Token
    , completionForm : Maybe UserExternalCompletionForm.Model
    , completingRegistration : ActionResult String
    , emailVerificationRequired : Bool
    }


initialModel : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Model
initialModel id error code sessionState state =
    { id = id
    , error = error
    , code = code
    , sessionState = sessionState
    , state = state
    , consent = False
    , authenticating = ActionResult.Loading
    , hash = Nothing
    , submittingConsent = ActionResult.Unset
    , originalUrl = ActionResult.Loading
    , originalState = ActionResult.Loading
    , token = ActionResult.Loading
    , completionForm = Nothing
    , completingRegistration = ActionResult.Unset
    , emailVerificationRequired = False
    }
