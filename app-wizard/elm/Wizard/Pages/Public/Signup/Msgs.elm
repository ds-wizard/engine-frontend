module Wizard.Pages.Public.Signup.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result ApiError ())
