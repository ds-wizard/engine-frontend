module UserManagement.Edit.Msgs exposing (..)

import Form
import Jwt
import UserManagement.Models exposing (User)


type Msg
    = GetUserCompleted (Result Jwt.JwtError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result Jwt.JwtError String)
    | PasswordFormMsg Form.Msg
    | PutPasswordCompleted (Result Jwt.JwtError String)
