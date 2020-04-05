module Wizard.Settings.Submission.Msgs exposing (..)

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetTemplatesCompleted (Result ApiError (List Template))
