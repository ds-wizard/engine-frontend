module Public.Signup.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result ApiError ())
