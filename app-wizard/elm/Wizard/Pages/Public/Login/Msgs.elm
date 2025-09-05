module Wizard.Pages.Public.Login.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = Email String
    | Password String
    | Code String
    | DoLogin
    | LoginCompleted (Result ApiError TokenResponse)
    | ExternalLoginOpenId OpenIDServiceConfig
