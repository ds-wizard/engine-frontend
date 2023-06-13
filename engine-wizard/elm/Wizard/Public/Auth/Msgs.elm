module Wizard.Public.Auth.Msgs exposing (Msg(..))

import Shared.Data.TokenResponse exposing (TokenResponse)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
