module Auth.Msgs exposing (..)

import Auth.Models exposing (User)
import Http
import Jwt


type Msg
    = Email String
    | Password String
    | Login
    | GetTokenCompleted (Result Http.Error String)
    | GetProfileCompleted (Result Jwt.JwtError User)
    | Logout
