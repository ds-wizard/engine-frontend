module Wizard.Apps.Create.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | PostAppComplete (Result ApiError ())
