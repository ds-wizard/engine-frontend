module Shared.Api.Questionnaires exposing
    ( cloneQuestionnaire
    , completeQuestionnaireMigration
    , deleteQuestionnaire
    , deleteQuestionnaireMigration
    , fetchQuestionnaireMigration
    , fetchSummaryReport
    , getQuestionnaire
    , getQuestionnaireMigration
    , getQuestionnaires
    , getSummaryReport
    , postQuestionnaire
    , putQuestionnaire
    , putQuestionnaireContent
    , putQuestionnaireMigration
    , websocket
    )

import Json.Encode exposing (Value)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtFetchEmpty, jwtGet, jwtOrHttpFetch, jwtOrHttpGet, jwtOrHttpPut, jwtPostEmpty, jwtPut, wsUrl)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Data.SummaryReport as SummaryReport exposing (SummaryReport)
import Uuid exposing (Uuid)


getQuestionnaires : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getQuestionnaires qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaires" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaires" Questionnaire.decoder)


getQuestionnaire : Uuid -> AbstractAppState a -> ToMsg QuestionnaireDetail msg -> Cmd msg
getQuestionnaire uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid) QuestionnaireDetail.decoder


getQuestionnaireMigration : Uuid -> AbstractAppState a -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration uuid =
    jwtGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current") QuestionnaireMigration.decoder


postQuestionnaire : Value -> AbstractAppState a -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" Questionnaire.decoder


cloneQuestionnaire : Uuid -> AbstractAppState a -> ToMsg Questionnaire msg -> Cmd msg
cloneQuestionnaire uuid =
    jwtFetchEmpty ("/questionnaires?cloneUuid=" ++ Uuid.toString uuid) Questionnaire.decoder


fetchQuestionnaireMigration : Uuid -> Value -> AbstractAppState a -> ToMsg QuestionnaireMigration msg -> Cmd msg
fetchQuestionnaireMigration uuid =
    jwtFetch ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations") QuestionnaireMigration.decoder


putQuestionnaire : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ Uuid.toString uuid)


putQuestionnaireContent : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireContent uuid =
    jwtOrHttpPut ("/questionnaires/" ++ Uuid.toString uuid ++ "/content")


putQuestionnaireMigration : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireMigration uuid =
    jwtPut ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current")


completeQuestionnaireMigration : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
completeQuestionnaireMigration uuid =
    jwtPostEmpty ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current/completion")


deleteQuestionnaireMigration : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteQuestionnaireMigration uuid =
    jwtDelete ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current")


deleteQuestionnaire : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteQuestionnaire uuid =
    jwtDelete ("/questionnaires/" ++ Uuid.toString uuid)


fetchSummaryReport : Uuid -> Value -> AbstractAppState a -> ToMsg SummaryReport msg -> Cmd msg
fetchSummaryReport questionnaireUuid =
    jwtOrHttpFetch ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/report/preview") SummaryReport.decoder


getSummaryReport : Uuid -> AbstractAppState a -> ToMsg SummaryReport msg -> Cmd msg
getSummaryReport questionnaireUuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/report") SummaryReport.decoder


websocket : Uuid -> AbstractAppState a -> String
websocket questionnaireUuid =
    wsUrl ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/websocket")
