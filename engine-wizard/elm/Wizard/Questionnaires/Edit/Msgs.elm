module Wizard.Questionnaires.Edit.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = FormMsg Form.Msg
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | PutQuestionnaireCompleted (Result ApiError ())
