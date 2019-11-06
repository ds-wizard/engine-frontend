module Wizard.Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Questionnaires.Common.Template exposing (Template)


type Msg
    = GetTemplatesCompleted (Result ApiError (List Template))
    | Close
    | SelectFormat String
    | SelectTemplate String
