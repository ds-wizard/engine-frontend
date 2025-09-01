module Wizard.Api.Questionnaires exposing
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

import Bool.Extra as Bool
import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Shared.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Api.WebSocket as WebSocket
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Document as Document exposing (Document)
import Wizard.Api.Models.Questionnaire as Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.QuestionnaireCommon as QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.QuestionnaireContent as QuestionnaireContent exposing (QuestionnaireContent)
import Wizard.Api.Models.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetailWrapper as QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireFile as QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Api.Models.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Api.Models.QuestionnairePreview as QuestionnairePreview exposing (QuestionnairePreview)
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Api.Models.QuestionnaireSettings as QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Api.Models.QuestionnaireSuggestion as QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Wizard.Api.Models.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Api.Models.SummaryReport as SummaryReport exposing (SummaryReport)
import Wizard.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Wizard.Data.AppState as AppState exposing (AppState)


getQuestionnaires : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Questionnaire) msg -> Cmd msg
getQuestionnaires appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/questionnaires" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaires" Questionnaire.decoder)


getQuestionnaireSuggestions : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireSuggestion) msg -> Cmd msg
getQuestionnaireSuggestions appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/questionnaires" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaires" QuestionnaireSuggestion.decoder)


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


getQuestionnaire : AppState -> Uuid -> ToMsg QuestionnaireCommon msg -> Cmd msg
getQuestionnaire appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid) QuestionnaireCommon.decoder


getQuestionnaireQuestionnaire : AppState -> Uuid -> ToMsg (QuestionnaireDetailWrapper QuestionnaireQuestionnaire) msg -> Cmd msg
getQuestionnaireQuestionnaire appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/questionnaire") (QuestionnaireDetailWrapper.decoder QuestionnaireQuestionnaire.decoder)


getQuestionnaireComments : AppState -> Uuid -> String -> ToMsg (Dict String (List CommentThread)) msg -> Cmd msg
getQuestionnaireComments appState questionnaireUuid path =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/comments?path=" ++ path) (D.dict (D.list CommentThread.decoder))


getQuestionnairePreview : AppState -> Uuid -> ToMsg (QuestionnaireDetailWrapper QuestionnairePreview) msg -> Cmd msg
getQuestionnairePreview appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/preview") (QuestionnaireDetailWrapper.decoder QuestionnairePreview.decoder)


getQuestionnaireSettings : AppState -> Uuid -> ToMsg (QuestionnaireDetailWrapper QuestionnaireSettings) msg -> Cmd msg
getQuestionnaireSettings appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/settings") (QuestionnaireDetailWrapper.decoder QuestionnaireSettings.decoder)


putQuestionnaireSettings : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putQuestionnaireSettings appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/settings") body


putQuestionnaireShare : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putQuestionnaireShare appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/share") body


getQuestionnaireUserSuggestions : AppState -> Uuid -> Bool -> String -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getQuestionnaireUserSuggestions appState questionnaireUuid editor query =
    let
        queryString =
            PaginationQueryString.fromQ query
                |> PaginationQueryString.withSize (Just 10)
                |> PaginationQueryString.toApiUrlWith [ ( "editor", Bool.toString editor ) ]

        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/users/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getQuestionnaireMigration : AppState -> Uuid -> ToMsg QuestionnaireMigration msg -> Cmd msg
getQuestionnaireMigration appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current") QuestionnaireMigration.decoder


getQuestionnaireVersions : AppState -> Uuid -> ToMsg (List QuestionnaireVersion) msg -> Cmd msg
getQuestionnaireVersions appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/versions") (D.list QuestionnaireVersion.decoder)


getQuestionnaireEvents : AppState -> Uuid -> ToMsg (List QuestionnaireEvent) msg -> Cmd msg
getQuestionnaireEvents appState uuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/events") (D.list QuestionnaireEvent.decoder)


getQuestionnaireEvent : AppState -> Uuid -> Uuid -> ToMsg QuestionnaireEvent msg -> Cmd msg
getQuestionnaireEvent appState uuid eventUuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/events/" ++ Uuid.toString eventUuid) QuestionnaireEvent.decoder


postQuestionnaire : AppState -> Value -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire appState body =
    Request.post (AppState.toServerInfo appState) "/questionnaires" Questionnaire.decoder body


postQuestionnaireFromTemplate : AppState -> Value -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaireFromTemplate appState body =
    Request.post (AppState.toServerInfo appState) "/questionnaires/from-template" Questionnaire.decoder body


cloneQuestionnaire : AppState -> Uuid -> ToMsg Questionnaire msg -> Cmd msg
cloneQuestionnaire appState uuid =
    Request.postEmptyBody (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/clone") Questionnaire.decoder


fetchQuestionnaireMigration : AppState -> Uuid -> Value -> ToMsg QuestionnaireMigration msg -> Cmd msg
fetchQuestionnaireMigration appState uuid body =
    Request.post (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations") QuestionnaireMigration.decoder body


putQuestionnaireContent : AppState -> Uuid -> List QuestionnaireEvent -> ToMsg () msg -> Cmd msg
putQuestionnaireContent appState uuid events =
    let
        body =
            E.object
                [ ( "events", E.list QuestionnaireEvent.encode events ) ]
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/content") body


putQuestionnaireMigration : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putQuestionnaireMigration appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current") body


completeQuestionnaireMigration : AppState -> Uuid -> ToMsg () msg -> Cmd msg
completeQuestionnaireMigration appState uuid =
    Request.postEmpty (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current/completion")


deleteQuestionnaireMigration : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteQuestionnaireMigration appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid ++ "/migrations/current")


deleteQuestionnaire : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteQuestionnaire appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString uuid)


getSummaryReport : AppState -> Uuid -> ToMsg (QuestionnaireDetailWrapper SummaryReport) msg -> Cmd msg
getSummaryReport appState questionnaireUuid =
    Request.get (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/report") (QuestionnaireDetailWrapper.decoder SummaryReport.decoder)


websocket : AppState -> Uuid -> String
websocket appState questionnaireUuid =
    let
        serverInfo =
            AppState.toServerInfo appState
    in
    case appState.config.signalBridge.webSocketUrl of
        Just webSocketUrl ->
            let
                token =
                    Maybe.map ((++) "Authorization=Bearer%20") serverInfo.token

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
            WebSocket.url serverInfo ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/websocket")


getDocumentPreview : AppState -> Uuid -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getDocumentPreview appState questionnaireUuid toMsg =
    let
        serverInfo =
            AppState.toServerInfo appState
    in
    Http.request
        { method = "GET"
        , headers = Request.authorizationHeaders serverInfo
        , url = serverInfo.apiUrl ++ "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = Request.expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getDocuments : AppState -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments appState questionnaireUuid _ qs =
    let
        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/documents" ++ PaginationQueryString.toApiUrl qs
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documents" Document.decoder)


getFiles : AppState -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireFile) msg -> Cmd msg
getFiles appState questionnaireUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/files" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "questionnaireFiles" QuestionnaireFile.decoder)


postVersion : AppState -> Uuid -> Value -> ToMsg QuestionnaireVersion msg -> Cmd msg
postVersion appState questionnaireUuid data =
    Request.post (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions") QuestionnaireVersion.decoder data


putVersion : AppState -> Uuid -> Uuid -> Value -> ToMsg QuestionnaireVersion msg -> Cmd msg
putVersion appState questionnaireUuid versionUuid data =
    Request.put (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions/" ++ Uuid.toString versionUuid) QuestionnaireVersion.decoder data


deleteVersion : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteVersion appState questionnaireUuid versionUuid =
    Request.delete (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/versions/" ++ Uuid.toString versionUuid)


fetchPreview : AppState -> Uuid -> Uuid -> ToMsg QuestionnaireContent msg -> Cmd msg
fetchPreview appState questionnaireUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.post (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/revert/preview") QuestionnaireContent.decoder body


postRevert : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
postRevert appState questionnaireUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/questionnaires/" ++ Uuid.toString questionnaireUuid ++ "/revert") body


getProjectTagsSuggestions : AppState -> PaginationQueryString -> List String -> ToMsg (Pagination String) msg -> Cmd msg
getProjectTagsSuggestions appState qs exclude =
    let
        queryString =
            PaginationQueryString.withSort (Just "projectTag") PaginationQueryString.SortASC qs
                |> PaginationQueryString.toApiUrlWith [ ( "exclude", String.join "," exclude ) ]

        url =
            "/questionnaires/project-tags/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectTags" D.string)
