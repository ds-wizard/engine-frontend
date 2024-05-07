module Wizard.Tenants.Create.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostAppComplete (Result ApiError ())
