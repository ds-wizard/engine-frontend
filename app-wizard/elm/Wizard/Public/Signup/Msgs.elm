module Wizard.Public.Signup.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result ApiError ())
