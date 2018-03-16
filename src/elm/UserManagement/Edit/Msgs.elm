module UserManagement.Edit.Msgs exposing (..)

import Form
import Jwt
import UserManagement.Common.Models exposing (User)
import UserManagement.Edit.Models exposing (View)


type Msg
    = GetUserCompleted (Result Jwt.JwtError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result Jwt.JwtError String)
    | PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result Jwt.JwtError String)
    | ChangeView View
