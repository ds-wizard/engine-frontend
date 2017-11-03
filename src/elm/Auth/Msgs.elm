module Auth.Msgs exposing (..)

import Http
import Jwt
import UserManagement.Models exposing (User)


type Msg
    = Email String
    | Password String
    | Login
    | GetTokenCompleted (Result Http.Error String)
    | GetProfileCompleted (Result Jwt.JwtError User)
    | Logout
