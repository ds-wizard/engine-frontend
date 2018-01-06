module Auth.Msgs exposing (..)

{-|

@docs Msg

-}

import Http
import Jwt
import UserManagement.Models exposing (User)


{-| -}
type Msg
    = Email String
    | Password String
    | Login
    | AuthUserCompleted (Result Http.Error String)
    | GetCurrentUserCompleted (Result Jwt.JwtError User)
    | Logout
