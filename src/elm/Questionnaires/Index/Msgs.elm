module Questionnaires.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Msgs as ExportModal


type Msg
    = GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
    | ShowExportQuestionnaire Questionnaire
    | ExportModalMsg ExportModal.Msg
    | DeleteQuestionnaireMigration String
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
