module UserManagement.Delete.Msgs exposing (..)

import Jwt
import UserManagement.Models exposing (User)


type Msg
    = GetUserCompleted (Result Jwt.JwtError User)
    | DeleteUser
    | DeleteUserCompleted (Result Jwt.JwtError String)
