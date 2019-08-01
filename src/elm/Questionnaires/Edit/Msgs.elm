module Questionnaires.Edit.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = FormMsg Form.Msg
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | PutQuestionnaireCompleted (Result ApiError ())
