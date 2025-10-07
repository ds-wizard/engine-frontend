module Wizard.Pages.Users.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Result exposing (Result)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostUserCompleted (Result ApiError ())
