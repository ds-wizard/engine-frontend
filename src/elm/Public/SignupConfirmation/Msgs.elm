module Public.SignupConfirmation.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)


type Msg
    = SendConfirmationCompleted (Result ApiError ())
