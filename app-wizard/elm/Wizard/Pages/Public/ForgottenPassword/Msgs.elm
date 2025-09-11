module Wizard.Pages.Public.ForgottenPassword.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result ApiError ())
