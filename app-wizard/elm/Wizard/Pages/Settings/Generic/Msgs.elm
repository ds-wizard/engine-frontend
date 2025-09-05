module Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form
import Wizard.Api.Models.EditableConfig exposing (EditableConfig)


type Msg
    = GetConfigCompleted (Result ApiError EditableConfig)
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
