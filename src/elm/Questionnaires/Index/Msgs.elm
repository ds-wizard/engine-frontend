module Questionnaires.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
    | ShowHideExportQuestionnaire (Maybe Questionnaire)
