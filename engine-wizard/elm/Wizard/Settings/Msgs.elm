module Wizard.Settings.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)


type Msg
    = GetApplicationConfigCompleted (Result ApiError EditableConfig)
    | PutApplicationConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
