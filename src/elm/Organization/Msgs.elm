module Organization.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import Organization.Models exposing (Organization)


type Msg
    = GetCurrentOrganizationCompleted (Result ApiError Organization)
    | PutCurrentOrganizationCompleted (Result ApiError ())
    | FormMsg Form.Msg
