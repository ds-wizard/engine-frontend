module Wizard.Pages.Settings.Usage.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Usage exposing (Usage)


type Msg
    = GetUsageComplete (Result ApiError Usage)
