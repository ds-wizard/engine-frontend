module Wizard.Public.Login.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Wizard.Api.Models.TokenResponse exposing (TokenResponse)


type Msg
    = Email String
    | Password String
    | Code String
    | DoLogin
    | LoginCompleted (Result ApiError TokenResponse)
    | ExternalLoginOpenId OpenIDServiceConfig
