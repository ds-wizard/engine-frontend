module Wizard.Settings.Generic.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.EditableConfig exposing (EditableConfig)


type Msg
    = GetConfigCompleted (Result ApiError EditableConfig)
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
