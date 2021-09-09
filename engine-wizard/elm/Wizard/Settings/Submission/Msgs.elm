module Wizard.Settings.Submission.Msgs exposing (Msg(..))

import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetTemplatesCompleted (Result ApiError (List Template))
