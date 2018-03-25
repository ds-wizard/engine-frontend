module Public.ForgottenPasswordConfirmation.Msgs exposing (..)

import Form
import Http


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result Http.Error String)
