module Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Common.Template exposing (Template)


type Msg
    = GetTemplatesCompleted (Result ApiError (List Template))
    | Close
    | SelectFormat String
    | SelectTemplate String
