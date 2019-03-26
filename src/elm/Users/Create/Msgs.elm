module Users.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import Result exposing (Result)


type Msg
    = FormMsg Form.Msg
    | PostUserCompleted (Result ApiError ())
