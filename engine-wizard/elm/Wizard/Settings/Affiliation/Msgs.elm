module Wizard.Settings.Affiliation.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)


type Msg
    = GetAffiliationConfigCompleted (Result ApiError EditableAffiliationConfig)
    | PutAffiliationConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
