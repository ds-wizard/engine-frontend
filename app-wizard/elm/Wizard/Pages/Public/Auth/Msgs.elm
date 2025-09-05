module Wizard.Pages.Public.Auth.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Json.Encode as E
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = GotOriginalUrl E.Value
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
