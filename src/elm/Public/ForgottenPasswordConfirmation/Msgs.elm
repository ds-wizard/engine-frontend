module Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result ApiError ())
