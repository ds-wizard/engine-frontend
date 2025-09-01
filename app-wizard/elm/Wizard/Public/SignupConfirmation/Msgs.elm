module Wizard.Public.SignupConfirmation.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)


type Msg
    = SendConfirmationCompleted (Result ApiError ())
