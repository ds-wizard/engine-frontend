module Questionnaires.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Msgs as ExportModal


type Msg
    = GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
    | ShowExportQuestionnaire Questionnaire
    | ExportModalMsg ExportModal.Msg
