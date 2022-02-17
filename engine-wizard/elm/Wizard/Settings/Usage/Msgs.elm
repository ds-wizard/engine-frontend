module Wizard.Settings.Usage.Msgs exposing (Msg(..))

import Shared.Data.Usage exposing (Usage)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetUsageComplete (Result ApiError Usage)
