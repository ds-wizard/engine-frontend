module Questionnaires.Detail.Msgs exposing (Msg(..))

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.Msgs
import Jwt
import KMEditor.Common.Models.Entities exposing (Level, Metric)


type Msg
    = GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
    | GetLevelsCompleted (Result Jwt.JwtError (List Level))
    | GetMetricsCompleted (Result Jwt.JwtError (List Metric))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result Jwt.JwtError String)
