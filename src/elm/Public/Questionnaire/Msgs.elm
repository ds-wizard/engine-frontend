module Public.Questionnaire.Msgs exposing (..)

import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.Msgs
import Http


type Msg
    = GetQuestionnaireCompleted (Result Http.Error QuestionnaireDetail)
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
