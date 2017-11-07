module Auth.Msgs exposing (..)

import Http
import Jwt
import UserManagement.Models exposing (User)


type Msg
    = Email String
    | Password String
    | Login
    | AuthUserCompleted (Result Http.Error String)
    | GetCurrentUserCompleted (Result Jwt.JwtError User)
    | Logout
