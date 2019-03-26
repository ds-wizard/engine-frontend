module Common.Api.Questionnaires exposing (deleteQuestionnaire, exportQuestionnaireUrl, fetchSummaryReport, getQuestionnaire, getQuestionnairePublic, getQuestionnaires, postQuestionnaire, putQuestionnaire)

import Common.Api exposing (ToMsg, httpGet, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport, summaryReportDecoder)
import Json.Encode exposing (Value)
import Questionnaires.Common.Models exposing (Questionnaire, questionnaireDecoder, questionnaireListDecoder)


getQuestionnaires : AppState -> ToMsg (List Questionnaire) msg -> Cmd msg
getQuestionnaires =
    jwtGet "/questionnaires" questionnaireListDecoder


getQuestionnaire : String -> AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnaire uuid =
    jwtGet ("/questionnaires/" ++ uuid) questionnaireDetailDecoder


getQuestionnairePublic : AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnairePublic =
    httpGet "/questionnaires/public" questionnaireDetailDecoder


postQuestionnaire : Value -> AppState -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" questionnaireDecoder


putQuestionnaire : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ uuid)


deleteQuestionnaire : String -> AppState -> ToMsg () msg -> Cmd msg
deleteQuestionnaire uuid =
    jwtDelete ("/questionnaires/" ++ uuid)


fetchSummaryReport : String -> Value -> AppState -> ToMsg SummaryReport msg -> Cmd msg
fetchSummaryReport questionnaireUuid =
    jwtFetch ("/questionnaires/" ++ questionnaireUuid ++ "/report/preview") summaryReportDecoder


exportQuestionnaireUrl : String -> String -> AppState -> String
exportQuestionnaireUrl uuid format appState =
    appState.apiUrl ++ "/questionnaires/" ++ uuid ++ "/dmp?format=" ++ format
