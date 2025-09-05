module Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form


type Msg
    = Cancel
    | FormMsg Form.Msg
    | PostAppComplete (Result ApiError ())
