module Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostAppComplete (Result ApiError ())
