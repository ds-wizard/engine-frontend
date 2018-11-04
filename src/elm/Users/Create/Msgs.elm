module Users.Create.Msgs exposing (Msg(..))

import Form
import Jwt


type Msg
    = FormMsg Form.Msg
    | PostUserCompleted (Result Jwt.JwtError String)
