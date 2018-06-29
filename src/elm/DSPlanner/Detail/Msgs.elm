module DSPlanner.Detail.Msgs exposing (..)

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.Msgs
import FormEngine.Msgs
import Jwt
import KMEditor.Common.Models.Entities exposing (Chapter)


type Msg
    = GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result Jwt.JwtError String)
