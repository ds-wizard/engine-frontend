module Users.Create.Msgs exposing (..)

import Form
import Jwt


type Msg
    = FormMsg Form.Msg
    | PostUserCompleted (Result Jwt.JwtError String)
