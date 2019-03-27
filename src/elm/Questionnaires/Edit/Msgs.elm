module Questionnaires.Edit.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form


type Msg
    = FormMsg Form.Msg
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | PutQuestionnaireCompleted (Result ApiError ())
