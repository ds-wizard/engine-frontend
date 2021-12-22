module Shared.Api.Questionnaires exposing
    ( cloneQuestionnaire
    , completeQuestionnaireMigration
    , deleteQuestionnaire
    , deleteQuestionnaireMigration
    , deleteVersion
    , documentPreviewUrl
    , fetchPreview
    , fetchQuestionnaireMigration
    , getDocumentPreview
    , getDocuments
    , getProjectTagsSuggestions
    , getQuestionnaire
    , getQuestionnaireMigration
    , getQuestionnaires
    , getSummaryReport
    , postQuestionnaire
    , postQuestionnaireFromTemplate
    , postRevert
    , postVersion
    , putQuestionnaire
    , putQuestionnaireContent
    , putQuestionnaireMigration
    , putVersion
    , websocket
    )

import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, authorizationHeaders, authorizedUrl, expectMetadata, jwtDelete, jwtFetch, jwtFetchEmpty, jwtFetchPut, jwtGet, jwtOrHttpFetch, jwtOrHttpGet, jwtOrHttpPut, jwtPost, jwtPostEmpty, jwtPut, wsUrl)
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters.FilterOperator as FilterOperator exposing (FilterOperator)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireContent as QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Data.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.SummaryReport as SummaryReport exposing (SummaryReport)
import Shared.Utils exposing (boolToString)
import Uuid exposing (Uuid)


type alias GetQuestionnairesFilters =
    { isTemplate : Maybe Bool
    , userUuids : Maybe String
    , userUuidsOp : Maybe FilterOperator
    , projectTags : Maybe String
    , projectTagsOp : Maybe FilterOperator
    }


getQuestionnaires : GetQuestionnairesFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getQuestionnaires filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams <|
                [ ( "isTemplate", Maybe.map boolToString filters.isTemplate )
                , ( "userUuids", filters.userUuids )
                , ( "userUuidsOp", Maybe.map FilterOperator.toString filters.userUuidsOp )
                , ( "projectTags", filters.projectTags )
                , ( "projectTagsOp", Maybe.map FilterOperator.toString filters.projectTagsOp )
                ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

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
    jwtOrHttpFetch "/questionnaires" Questionnaire.decoder


postQuestionnaireFromTemplate : Value -> AbstractAppState a -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaireFromTemplate =
    jwtOrHttpFetch "/questionnaires/from-template" Questionnaire.decoder


cloneQuestionnaire : Uuid -> AbstractAppState a -> ToMsg Questionnaire msg -> Cmd msg
cloneQuestionnaire uuid =
    jwtFetchEmpty ("/questionnaires/" ++ Uuid.toString uuid ++ "/clone") Questionnaire.decoder


fetchQuestionnaireMigration : Uuid -> Value -> AbstractAppState a -> ToMsg QuestionnaireMigration msg -> Cmd msg
fetchQuestionnaireMigration uuid =
    jwtFetch ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations") QuestionnaireMigration.decoder


putQuestionnaire : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaire uuid =
    jwtPut ("/questionnaires/" ++ Uuid.toString uuid)


putQuestionnaireContent : Uuid -> List QuestionnaireEvent -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireContent uuid events =
    let
        body =
            E.object
                [ ( "events", E.list QuestionnaireEvent.encode events ) ]
    in
    jwtOrHttpPut ("/questionnaires/" ++ Uuid.toString uuid ++ "/content") body


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


getSummaryReport : Uuid -> AbstractAppState a -> ToMsg SummaryReport msg -> Cmd msg
getSummaryReport questionnaireUuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/report") SummaryReport.decoder


websocket : Uuid -> AbstractAppState a -> String
websocket questionnaireUuid =
    wsUrl ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/websocket")


getDocumentPreview : Uuid -> AbstractAppState a -> ToMsg Http.Metadata msg -> Cmd msg
getDocumentPreview questionnaireUuid appState toMsg =
    Http.request
        { method = "GET"
        , headers = authorizationHeaders appState
        , url = appState.apiUrl ++ "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = expectMetadata toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


documentPreviewUrl : Uuid -> AbstractAppState a -> String
documentPreviewUrl questionnaireUuid =
    authorizedUrl ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents/preview")


getDocuments : Uuid -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments questionnaireUuid qs =
    let
        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents" ++ PaginationQueryString.toApiUrl qs
    in
    jwtOrHttpGet url (Pagination.decoder "documents" Document.decoder)


postVersion : Uuid -> Value -> AbstractAppState a -> ToMsg QuestionnaireVersion msg -> Cmd msg
postVersion questionnaireUuid data =
    jwtFetch ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions") QuestionnaireVersion.decoder data


putVersion : Uuid -> Uuid -> Value -> AbstractAppState a -> ToMsg QuestionnaireVersion msg -> Cmd msg
putVersion questionnaireUuid versionUuid data =
    jwtFetchPut ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions/" ++ Uuid.toString versionUuid) QuestionnaireVersion.decoder data


deleteVersion : Uuid -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteVersion questionnaireUuid versionUuid =
    jwtDelete ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions/" ++ Uuid.toString versionUuid)


fetchPreview : Uuid -> Uuid -> AbstractAppState a -> ToMsg QuestionnaireContent msg -> Cmd msg
fetchPreview questionnaireUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    jwtOrHttpFetch ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/revert/preview") QuestionnaireContent.decoder body


postRevert : Uuid -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postRevert questionnaireUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    jwtPost ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/revert") body


getProjectTagsSuggestions : PaginationQueryString -> List String -> AbstractAppState a -> ToMsg (Pagination String) msg -> Cmd msg
getProjectTagsSuggestions qs exclude =
    let
        queryString =
            PaginationQueryString.withSort (Just "projectTag") PaginationQueryString.SortASC qs
                |> PaginationQueryString.toApiUrlWith [ ( "exclude", String.join "," exclude ) ]

        url =
            "/questionnaires/project-tags/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "projectTags" D.string)
