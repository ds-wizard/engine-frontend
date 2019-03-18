module Questionnaires.Edit.Msgs exposing (Msg(..))

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form
import Jwt


type Msg
    = FormMsg Form.Msg
    | GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
    | PutQuestionnaireCompleted (Result Jwt.JwtError String)
