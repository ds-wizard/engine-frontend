module Wizard.Pages.Public.Auth.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Ports.LocalStorage as LocalStorage
import Json.Decode as D
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = GotOriginalUrl (Result D.Error (LocalStorage.Item (Maybe String)))
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
