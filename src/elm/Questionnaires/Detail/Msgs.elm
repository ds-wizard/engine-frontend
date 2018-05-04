module Questionnaires.Detail.Msgs exposing (..)

import Jwt
import Questionnaires.Common.Models exposing (QuestionnaireDetail)


type Msg
    = GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
