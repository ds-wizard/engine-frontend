module Wizard.Users.Create.Msgs exposing (Msg(..))

import Form
import Result exposing (Result)
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostUserCompleted (Result ApiError ())
