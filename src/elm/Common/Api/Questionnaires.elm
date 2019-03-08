module Common.Api.Questionnaires exposing
    ( deleteQuestionnaire
    , deleteQuestionnaireMigration
    , exportQuestionnaireUrl
    , fetchQuestionnaireMigration
    , fetchSummaryReport
    , getQuestionnaire
    , getQuestionnaireMigration
    , getQuestionnairePublic
    , getQuestionnaires
    , postQuestionnaire
    , putQuestionnaire
    , putQuestionnaireMigration
    )

import Common.Api exposing (ToMsg, httpGet, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport, summaryReportDecoder)
import Json.Decode as D
import Json.Encode exposing (Value)
import Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Questionnaires.Common.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)


getQuestionnaires : AppState -> ToMsg (List Questionnaire) msg -> Cmd msg
getQuestionnaires =
    jwtGet "/questionnaires" (D.list Questionnaire.decoder)


getQuestionnaire : String -> AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnaire uuid =
    jwtGet ("/questionnaires/" ++ uuid) questionnaireDetailDecoder


getQuestionnaireMigration : String -> AppState -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration uuid =
    jwtGet ("/questionnaires/" ++ uuid ++ "/migrations/current") QuestionnaireMigration.decoder


getQuestionnairePublic : AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnairePublic =
    httpGet "/questionnaires/public" questionnaireDetailDecoder


postQuestionnaire : Value -> AppState -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" Questionnaire.decoder


fetchQuestionnaireMigration : String -> Value -> AppState -> ToMsg QuestionnaireMigration msg -> Cmd msg
fetchQuestionnaireMigration uuid =
    jwtFetch ("/questionnaires/" ++ uuid ++ "/migrations") QuestionnaireMigration.decoder


putQuestionnaire : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ uuid)


putQuestionnaireMigration : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaireMigration uuid =
    jwtPut ("/questionnaires/" ++ uuid ++ "/migrations/current")


deleteQuestionnaireMigration : String -> AppState -> ToMsg () msg -> Cmd msg
deleteQuestionnaireMigration uuid =
    jwtDelete ("/questionnaires/" ++ uuid ++ "/migrations/current")


deleteQuestionnaire : String -> AppState -> ToMsg () msg -> Cmd msg
deleteQuestionnaire uuid =
    jwtDelete ("/questionnaires/" ++ uuid)


fetchSummaryReport : String -> Value -> AppState -> ToMsg SummaryReport msg -> Cmd msg
fetchSummaryReport questionnaireUuid =
    jwtFetch ("/questionnaires/" ++ questionnaireUuid ++ "/report/preview") summaryReportDecoder


exportQuestionnaireUrl : String -> String -> Maybe String -> AppState -> String
exportQuestionnaireUrl uuid format templateUuid appState =
    let
        url =
            appState.apiUrl ++ "/questionnaires/" ++ uuid ++ "/dmp?format=" ++ format
    in
    templateUuid
        |> Maybe.map ((++) (url ++ "&templateUuid="))
        |> Maybe.withDefault url
