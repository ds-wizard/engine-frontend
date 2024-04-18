module Wizard.Public.Auth.Msgs exposing (Msg(..))

import Json.Encode as E
import Shared.Data.TokenResponse exposing (TokenResponse)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GotOriginalUrl E.Value
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
