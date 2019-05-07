module Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Index.ExportModal.Models exposing (Template)


type Msg
    = GetTemplatesCompleted (Result ApiError (List Template))
    | Close
    | SelectFormat String
    | SelectTemplate String
