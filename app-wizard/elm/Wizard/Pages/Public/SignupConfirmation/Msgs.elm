module Wizard.Pages.Public.SignupConfirmation.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)


type Msg
    = SendConfirmationCompleted (Result ApiError ())
