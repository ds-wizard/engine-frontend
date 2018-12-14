module Public.Questionnaire.Requests exposing (getQuestionnaire)

import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Http
import Requests exposing (apiUrl)


getQuestionnaire : Http.Request QuestionnaireDetail
getQuestionnaire =
    Http.get (apiUrl "/questionnaires/public") questionnaireDetailDecoder
