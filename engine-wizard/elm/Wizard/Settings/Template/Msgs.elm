module Wizard.Settings.Template.Msgs exposing (Msg(..))

import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetTemplatesComplete (Result ApiError (List TemplateSuggestion))
