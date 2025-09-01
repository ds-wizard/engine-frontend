module Wizard.Pages.Public.Auth.Msgs exposing (Msg(..))

import Json.Encode as E
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = GotOriginalUrl E.Value
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
