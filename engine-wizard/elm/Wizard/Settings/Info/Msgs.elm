module Wizard.Settings.Info.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)


type Msg
    = GetInfoConfigCompleted (Result ApiError EditableInfoConfig)
    | PutInfoConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
