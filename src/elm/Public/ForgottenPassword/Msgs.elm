module Public.ForgottenPassword.Msgs exposing (..)

import Form
import Http


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result Http.Error String)
