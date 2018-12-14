module Public.Signup.Msgs exposing (Msg(..))

import Form
import Http


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result Http.Error String)
