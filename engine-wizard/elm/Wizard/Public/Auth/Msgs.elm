module Wizard.Public.Auth.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)


type Msg
    = AuthenticationCompleted (Result ApiError String)
