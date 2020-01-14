module Wizard.Public.ForgottenPassword.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result ApiError ())
