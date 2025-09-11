module Wizard.Pages.Settings.Usage.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.Usage exposing (Usage)


type Msg
    = GetUsageComplete (Result ApiError Usage)
