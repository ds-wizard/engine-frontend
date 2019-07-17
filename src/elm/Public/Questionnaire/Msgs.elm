module Public.Questionnaire.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Msgs
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
