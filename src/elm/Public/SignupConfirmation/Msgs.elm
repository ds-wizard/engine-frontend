module Public.SignupConfirmation.Msgs exposing (..)

import Http


type Msg
    = SendConfirmationCompleted (Result Http.Error String)
