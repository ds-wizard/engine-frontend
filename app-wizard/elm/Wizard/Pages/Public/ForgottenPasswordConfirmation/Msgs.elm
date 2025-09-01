module Wizard.Pages.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result ApiError ())
