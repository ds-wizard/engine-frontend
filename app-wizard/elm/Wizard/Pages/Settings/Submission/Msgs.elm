module Wizard.Pages.Settings.Submission.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.DocumentTemplateAllSuggestion exposing (DocumentTemplateAllSuggestion)
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetTemplatesCompleted (Result ApiError (List DocumentTemplateAllSuggestion))
