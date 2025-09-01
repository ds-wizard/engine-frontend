module Wizard.Pages.Public.ForgottenPassword.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result ApiError ())
