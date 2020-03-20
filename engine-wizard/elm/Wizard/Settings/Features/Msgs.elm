module Wizard.Settings.Features.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)


type Msg
    = GetFeaturesConfigCompleted (Result ApiError EditableFeaturesConfig)
    | PutFeaturesConfigCompleted (Result ApiError ())
    | FormMsg Form.Msg
