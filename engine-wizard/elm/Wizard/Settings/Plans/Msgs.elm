module Wizard.Settings.Plans.Msgs exposing (Msg(..))

import Shared.Data.Plan exposing (Plan)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetPlansComplete (Result ApiError (List Plan))
