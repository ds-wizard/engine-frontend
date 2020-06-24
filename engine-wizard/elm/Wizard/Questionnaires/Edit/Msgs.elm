module Wizard.Questionnaires.Edit.Msgs exposing (Msg(..))

import Form
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | PutQuestionnaireCompleted (Result ApiError ())
