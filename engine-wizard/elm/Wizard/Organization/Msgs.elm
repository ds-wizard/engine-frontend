module Wizard.Organization.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Organization.Common.Organization exposing (Organization)


type Msg
    = GetCurrentOrganizationCompleted (Result ApiError Organization)
    | PutCurrentOrganizationCompleted (Result ApiError ())
    | FormMsg Form.Msg
