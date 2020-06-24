module Wizard.Settings.Generic.Msgs exposing (..)

import Form
import Shared.Data.EditableConfig exposing (EditableConfig)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetConfigCompleted (Result ApiError EditableConfig)
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
