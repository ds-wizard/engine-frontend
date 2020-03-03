module Wizard.Auth.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Users.Common.User exposing (User)


type Msg
    = GetCurrentUserCompleted (Maybe String) (Result ApiError User)
    | Logout
    | Token String JwtToken (Maybe String)
