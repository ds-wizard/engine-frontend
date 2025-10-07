module Wizard.Pages.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result ApiError ())
