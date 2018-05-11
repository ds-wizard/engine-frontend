module Users.Edit.Msgs exposing (..)

import Form
import Jwt
import Users.Common.Models exposing (User)
import Users.Edit.Models exposing (View)


type Msg
    = GetUserCompleted (Result Jwt.JwtError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result Jwt.JwtError String)
    | PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result Jwt.JwtError String)
    | ChangeView View
