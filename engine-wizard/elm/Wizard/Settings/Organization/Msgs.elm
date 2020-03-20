module Wizard.Settings.Organization.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableOrganizationConfig exposing (EditableOrganizationConfig)


type Msg
    = GetOrganizationConfigCompleted (Result ApiError EditableOrganizationConfig)
    | PutOrganizationConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
