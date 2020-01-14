module Wizard.Public.Questionnaire.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | QuestionnaireMsg Wizard.Common.Questionnaire.Msgs.Msg
