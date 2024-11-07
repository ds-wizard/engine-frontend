module Shared.Api.Questionnaires exposing
    ( cloneQuestionnaire
    , completeQuestionnaireMigration
    , deleteQuestionnaire
    , deleteQuestionnaireMigration
    , deleteVersion
    , fetchPreview
    , fetchQuestionnaireMigration
    , getDocumentPreview
    , getDocuments
    , getFiles
    , getProjectTagsSuggestions
    , getQuestionnaire
    , getQuestionnaireComments
    , getQuestionnaireEvent
    , getQuestionnaireEvents
    , getQuestionnaireMigration
    , getQuestionnairePreview
    , getQuestionnaireQuestionnaire
    , getQuestionnaireSettings
    , getQuestionnaireSuggestions
    , getQuestionnaireUserSuggestions
    , getQuestionnaireVersions
    , getQuestionnaires
    , getSummaryReport
    , postQuestionnaire
    , postQuestionnaireFromTemplate
    , postRevert
    , postVersion
    , putQuestionnaireContent
    , putQuestionnaireMigration
    , putQuestionnaireSettings
    , putQuestionnaireShare
    , putVersion
    , websocket
    )

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, authorizationHeaders, expectMetadataAndJson, jwtDelete, jwtFetch, jwtFetchEmpty, jwtFetchPut, jwtGet, jwtOrHttpFetch, jwtOrHttpGet, jwtOrHttpPut, jwtPost, jwtPostEmpty, jwtPut, wsUrl)
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireCommon as QuestionnaireCommon exposing (QuestionnaireCommon)
import Shared.Data.QuestionnaireContent as QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetailWrapper as QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Shared.Data.QuestionnaireFile as QuestionnaireFile exposing (QuestionnaireFile)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Data.QuestionnairePreview as QuestionnairePreview exposing (QuestionnairePreview)
import Shared.Data.QuestionnaireQuestionnaire as QuestionnaireDetail exposing (QuestionnaireQuestionnaire)
import Shared.Data.QuestionnaireSettings as QuestionnaireSettings exposing (QuestionnaireSettings)
import Shared.Data.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Shared.Data.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.SummaryReport as SummaryReport exposing (SummaryReport)
import Shared.Data.UrlResponse as UrlResponse exposing (UrlResponse)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Shared.Utils exposing (boolToString)
import String.Extra as String
import Uuid exposing (Uuid)


getQuestionnaires : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getQuestionnaires filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/questionnaires" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaires" Questionnaire.decoder)


getQuestionnaireSuggestions : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireSuggestion) msg -> Cmd msg
getQuestionnaireSuggestions filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/questionnaires" ++ queryString
    in
    jwtGet url (Pagination.decoder "questionnaires" QuestionnaireSuggestion.decoder)


createListExtraParams : PaginationQueryFilters -> List ( String, String )
createListExtraParams filters =
    PaginationQueryString.filterParams
        [ ( "isTemplate", PaginationQueryFilters.getValue "isTemplate" filters )
        , ( "isMigrating", PaginationQueryFilters.getValue "isMigrating" filters )
        , ( "userUuids", PaginationQueryFilters.getValue "userUuids" filters )
        , ( "userUuidsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "userUuids" filters) )
        , ( "projectTags", PaginationQueryFilters.getValue "projectTags" filters )
        , ( "projectTagsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "projectTags" filters) )
        , ( "packageIds", PaginationQueryFilters.getValue "packages" filters )
        , ( "packageIdsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "packages" filters) )
        ]


getQuestionnaire : Uuid -> AbstractAppState a -> ToMsg QuestionnaireCommon msg -> Cmd msg
getQuestionnaire uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid) QuestionnaireCommon.decoder


getQuestionnaireQuestionnaire : Uuid -> AbstractAppState a -> ToMsg (QuestionnaireDetailWrapper QuestionnaireQuestionnaire) msg -> Cmd msg
getQuestionnaireQuestionnaire uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/questionnaire") (QuestionnaireDetailWrapper.decoder QuestionnaireDetail.decoder)


getQuestionnaireComments : Uuid -> String -> AbstractAppState a -> ToMsg (Dict String (List CommentThread)) msg -> Cmd msg
getQuestionnaireComments questionnaireUuid path =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/comments?path=" ++ path) (D.dict (D.list CommentThread.decoder))


getQuestionnairePreview : Uuid -> AbstractAppState a -> ToMsg (QuestionnaireDetailWrapper QuestionnairePreview) msg -> Cmd msg
getQuestionnairePreview uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/preview") (QuestionnaireDetailWrapper.decoder QuestionnairePreview.decoder)


getQuestionnaireSettings : Uuid -> AbstractAppState a -> ToMsg (QuestionnaireDetailWrapper QuestionnaireSettings) msg -> Cmd msg
getQuestionnaireSettings uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/settings") (QuestionnaireDetailWrapper.decoder QuestionnaireSettings.decoder)


putQuestionnaireSettings : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireSettings uuid =
    jwtPut ("/questionnaires/" ++ Uuid.toString uuid ++ "/settings")


putQuestionnaireShare : Uuid -> Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putQuestionnaireShare uuid =
    jwtPut ("/questionnaires/" ++ Uuid.toString uuid ++ "/share")


getQuestionnaireUserSuggestions : Uuid -> Bool -> String -> AbstractAppState a -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getQuestionnaireUserSuggestions questionnaireUuid editor query =
    let
        queryString =
            PaginationQueryString.fromQ query
                |> PaginationQueryString.withSize (Just 10)
                |> PaginationQueryString.toApiUrlWith [ ( "editor", boolToString editor ) ]
    in
    jwtGet ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/users/suggestions" ++ queryString) (Pagination.decoder "users" UserSuggestion.decoder)


getQuestionnaireMigration : Uuid -> AbstractAppState a -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration uuid =
    jwtGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current") QuestionnaireMigration.decoder


getQuestionnaireVersions : Uuid -> AbstractAppState a -> ToMsg (List QuestionnaireVersion) msg -> Cmd msg
getQuestionnaireVersions uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/versions") (D.list QuestionnaireVersion.decoder)


getQuestionnaireEvents : Uuid -> AbstractAppState a -> ToMsg (List QuestionnaireEvent) msg -> Cmd msg
getQuestionnaireEvents uuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/events") (D.list QuestionnaireEvent.decoder)


getQuestionnaireEvent : Uuid -> Uuid -> AbstractAppState a -> ToMsg QuestionnaireEvent msg -> Cmd msg
getQuestionnaireEvent uuid eventUuid =
    jwtGet ("/questionnaires/" ++ Uuid.toString uuid ++ "/events/" ++ Uuid.toString eventUuid) QuestionnaireEvent.decoder


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


getSummaryReport : Uuid -> AbstractAppState a -> ToMsg (QuestionnaireDetailWrapper SummaryReport) msg -> Cmd msg
getSummaryReport questionnaireUuid =
    jwtOrHttpGet ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/report") (QuestionnaireDetailWrapper.decoder SummaryReport.decoder)


websocket : Uuid -> AbstractAppState a -> String
websocket questionnaireUuid appState =
    case appState.config.signalBridge.webSocketUrl of
        Just webSocketUrl ->
            let
                token =
                    Maybe.map ((++) "Authorization=Bearer%20") (String.toMaybe appState.session.token.token)

                queryParams =
                    List.filterMap identity
                        [ token
                        , Just "subscription=Questionnaire"
                        , Just ("identifier=" ++ Uuid.toString questionnaireUuid)
                        ]

                queryString =
                    String.join "&" queryParams
            in
            webSocketUrl ++ "?" ++ queryString

        Nothing ->
            wsUrl ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/websocket") appState


getDocumentPreview : Uuid -> AbstractAppState a -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getDocumentPreview questionnaireUuid appState toMsg =
    Http.request
        { method = "GET"
        , headers = authorizationHeaders appState
        , url = appState.apiUrl ++ "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getDocuments : Uuid -> PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments questionnaireUuid _ qs =
    let
        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents" ++ PaginationQueryString.toApiUrl qs
    in
    jwtOrHttpGet url (Pagination.decoder "documents" Document.decoder)


getFiles : Uuid -> PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireFile) msg -> Cmd msg
getFiles questionnaireUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files" ++ queryString
    in
    jwtOrHttpGet url (Pagination.decoder "questionnaireFiles" QuestionnaireFile.decoder)


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
