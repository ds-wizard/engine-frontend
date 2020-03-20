module Wizard.Settings.Client.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)


type Msg
    = GetClientConfigCompleted (Result ApiError EditableClientConfig)
    | PutClientConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
