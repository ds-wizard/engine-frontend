module Wizard.Public.Auth.Msgs exposing (Msg(..))

import Shared.Data.Token exposing (Token)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = AuthenticationCompleted (Result ApiError Token)
