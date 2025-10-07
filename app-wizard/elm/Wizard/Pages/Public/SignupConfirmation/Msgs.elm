module Wizard.Pages.Public.SignupConfirmation.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)


type Msg
    = SendConfirmationCompleted (Result ApiError ())
