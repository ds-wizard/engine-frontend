module Wizard.Pages.Public.OpenIdCallback.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UserFromExternal exposing (UserFromExternal)
import Common.Components.UserExternalCompletionForm as UserExternalCompletionForm
import Common.Ports.LocalStorage as LocalStorage
import Json.Decode as D
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = GotOriginalUrl (Result D.Error (LocalStorage.Item (Maybe String)))
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
    | CompletionFormMsg UserExternalCompletionForm.Msg
    | SubmitCompletionForm UserFromExternal
    | SubmitCompletionFormCompleted (Result ApiError TokenResponse)
