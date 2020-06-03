module Wizard.Settings.Template.Msgs exposing (..)

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetTemplatesComplete (Result ApiError (List Template))
