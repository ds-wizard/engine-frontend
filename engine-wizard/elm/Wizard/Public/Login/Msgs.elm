module Wizard.Public.Login.Msgs exposing (Msg(..))

import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.TokenResponse exposing (TokenResponse)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = Email String
    | Password String
    | Code String
    | DoLogin
    | LoginCompleted (Result ApiError TokenResponse)
    | ExternalLoginOpenId OpenIDServiceConfig
