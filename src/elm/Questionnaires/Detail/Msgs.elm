module Questionnaires.Detail.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.Msgs
import KMEditor.Common.Models.Entities exposing (Level, Metric)


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | GetLevelsCompleted (Result ApiError (List Level))
    | GetMetricsCompleted (Result ApiError (List Metric))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result ApiError ())
