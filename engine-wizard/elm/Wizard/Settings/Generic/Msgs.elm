module Wizard.Settings.Generic.Msgs exposing (..)

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)


type Msg
    = GetConfigCompleted (Result ApiError EditableConfig)
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
