module Public.Signup.Msgs exposing (..)

import Form
import Http


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result Http.Error String)
