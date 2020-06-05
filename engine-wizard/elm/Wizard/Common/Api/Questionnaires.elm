module Wizard.Common.Api.Questionnaires exposing
    ( cloneQuestionnaire
    , completeQuestionnaireMigration
    , deleteQuestionnaire
    , deleteQuestionnaireMigration
    , exportQuestionnaireUrl
    , fetchQuestionnaireMigration
    , fetchSummaryReport
    , getQuestionnaire
    , getQuestionnaireMigration
    , getQuestionnaires
    , getQuestionnairesTracker
    , postQuestionnaire
    , putQuestionnaire
    , putQuestionnaireMigration
    )

import Json.Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtFetchEmpty, jwtGet, jwtGetWithTracker, jwtPostEmpty, jwtPut)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Pagination.Pagination as Pagination exposing (Pagination)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Wizard.Common.Questionnaire.Models.SummaryReport exposing (SummaryReport, summaryReportDecoder)
import Wizard.Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)


getQuestionnairesTracker : String
getQuestionnairesTracker =
    "getQuestionnairesTracker"


getQuestionnaires : PaginationQueryString -> AppState -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getQuestionnaires qs =
    jwtGetWithTracker
        getQuestionnairesTracker
        ("/questionnaires" ++ PaginationQueryString.toApiUrl qs)
        (Pagination.decoder "questionnaires" Questionnaire.decoder)


getQuestionnaire : String -> AppState -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnaire uuid =
    jwtGet ("/questionnaires/" ++ uuid) QuestionnaireDetail.decoder


getQuestionnaireMigration : String -> AppState -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration uuid =
    jwtGet ("/questionnaires/" ++ uuid ++ "/migrations/current") QuestionnaireMigration.decoder


postQuestionnaire : Value -> AppState -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" Questionnaire.decoder


cloneQuestionnaire : String -> AppState -> ToMsg Questionnaire msg -> Cmd msg
cloneQuestionnaire uuid =
    jwtFetchEmpty ("/questionnaires?cloneUuid=" ++ uuid) Questionnaire.decoder


fetchQuestionnaireMigration : String -> Value -> AppState -> ToMsg QuestionnaireMigration msg -> Cmd msg
fetchQuestionnaireMigration uuid =
    jwtFetch ("/questionnaires/" ++ uuid ++ "/migrations") QuestionnaireMigration.decoder


putQuestionnaire : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ uuid)


putQuestionnaireMigration : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putQuestionnaireMigration uuid =
    jwtPut ("/questionnaires/" ++ uuid ++ "/migrations/current")


completeQuestionnaireMigration : String -> AppState -> ToMsg () msg -> Cmd msg
completeQuestionnaireMigration uuid =
    jwtPostEmpty ("/questionnaires/" ++ uuid ++ "/migrations/current/completion")


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
