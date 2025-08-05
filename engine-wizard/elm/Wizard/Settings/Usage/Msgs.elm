module Wizard.Settings.Usage.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Usage exposing (Usage)


type Msg
    = GetUsageComplete (Result ApiError Usage)
