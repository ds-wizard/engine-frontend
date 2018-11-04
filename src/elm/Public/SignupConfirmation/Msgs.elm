module Public.SignupConfirmation.Msgs exposing (Msg(..))

import Http


type Msg
    = SendConfirmationCompleted (Result Http.Error String)
