module Wizard.Settings.Generic.Msgs exposing (..)

import Form
import Shared.Error.ApiError exposing (ApiError)


type Msg config
    = GetConfigCompleted (Result ApiError config)
    | PutConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
