module Wizard.Public.SignupConfirmation.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)


type Msg
    = SendConfirmationCompleted (Result ApiError ())
