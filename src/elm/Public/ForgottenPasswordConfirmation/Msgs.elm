module Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))

import Form
import Http


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result Http.Error String)
