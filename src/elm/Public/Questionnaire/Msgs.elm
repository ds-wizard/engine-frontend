module Public.Questionnaire.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.Msgs


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
