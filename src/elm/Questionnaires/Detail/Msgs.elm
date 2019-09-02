module Questionnaires.Detail.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Msgs
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | GetLevelsCompleted (Result ApiError (List Level))
    | GetMetricsCompleted (Result ApiError (List Metric))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result ApiError ())
