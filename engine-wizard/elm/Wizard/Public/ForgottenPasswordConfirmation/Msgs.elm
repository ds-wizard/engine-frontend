module Wizard.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result ApiError ())
