module Auth.Msgs exposing (..)

import Auth.Models exposing (JwtToken)
import Jwt
import Users.Common.Models exposing (User)


type Msg
    = GetCurrentUserCompleted (Result Jwt.JwtError User)
    | Logout
    | Token String JwtToken
