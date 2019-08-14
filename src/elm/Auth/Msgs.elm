module Auth.Msgs exposing (Msg(..))

import Auth.Models exposing (JwtToken)
import Common.ApiError exposing (ApiError)
import Users.Common.User exposing (User)


type Msg
    = GetCurrentUserCompleted (Result ApiError User)
    | Logout
    | Token String JwtToken
