module Wizard.Api.Projects exposing
    ( clone
    , delete
    , deleteMigration
    , deleteVersion
    , fetchMigration
    , fetchPreview
    , get
    , getCommentThreads
    , getDocumentPreview
    , getDocuments
    , getEvent
    , getEvents
    , getFiles
    , getList
    , getMigration
    , getPreview
    , getProjectTagsSuggestions
    , getQuestionnaire
    , getSettings
    , getSuggestions
    , getSummaryReport
    , getUserSuggestions
    , getVersions
    , post
    , postFromTemplate
    , postMigrationCompletion
    , postRevert
    , postVersion
    , putContent
    , putMigration
    , putSettings
    , putShare
    , putVersion
    , websocket
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Api.WebSocket as WebSocket
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Document as Document exposing (Document)
import Wizard.Api.Models.Project as Project exposing (Project)
import Wizard.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectContent as ProjectContent exposing (ProjectContent)
import Wizard.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetailWrapper as ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectFile as ProjectFile exposing (ProjectFile)
import Wizard.Api.Models.ProjectMigration as ProjectMigration exposing (ProjectMigration)
import Wizard.Api.Models.ProjectPreview as ProjectPreview exposing (ProjectPreview)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.ProjectSettings as ProjectSettings exposing (ProjectSettings)
import Wizard.Api.Models.ProjectSuggestion as ProjectSuggestion exposing (ProjectSuggestion)
import Wizard.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)
import Wizard.Api.Models.SummaryReport as SummaryReport exposing (SummaryReport)
import Wizard.Data.AppState as AppState exposing (AppState)


getList : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Project) msg -> Cmd msg
getList appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/projects" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projects" Project.decoder)


getSuggestions : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectSuggestion) msg -> Cmd msg
getSuggestions appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/projects" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projects" ProjectSuggestion.decoder)


createListExtraParams : PaginationQueryFilters -> List ( String, String )
createListExtraParams filters =
    PaginationQueryString.filterParams
        [ ( "isTemplate", PaginationQueryFilters.getValue "isTemplate" filters )
        , ( "isMigrating", PaginationQueryFilters.getValue "isMigrating" filters )
        , ( "userUuids", PaginationQueryFilters.getValue "userUuids" filters )
        , ( "userUuidsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "userUuids" filters) )
        , ( "projectTags", PaginationQueryFilters.getValue "projectTags" filters )
        , ( "projectTagsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "projectTags" filters) )
        , ( "knowledgeModelPackageIds", PaginationQueryFilters.getValue "knowledgeModelPackages" filters )
        , ( "knowledgeModelPackageIdsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "knowledgeModelPackages" filters) )
        ]


get : AppState -> Uuid -> ToMsg ProjectCommon msg -> Cmd msg
get appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid) ProjectCommon.decoder


getQuestionnaire : AppState -> Uuid -> ToMsg (ProjectDetailWrapper ProjectQuestionnaire) msg -> Cmd msg
getQuestionnaire appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/questionnaire") (ProjectDetailWrapper.decoder ProjectQuestionnaire.decoder)


getCommentThreads : AppState -> Uuid -> String -> ToMsg (Dict String (List CommentThread)) msg -> Cmd msg
getCommentThreads appState projectUuid path =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/comments?path=" ++ path) (D.dict (D.list CommentThread.decoder))


getPreview : AppState -> Uuid -> ToMsg (ProjectDetailWrapper ProjectPreview) msg -> Cmd msg
getPreview appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/preview") (ProjectDetailWrapper.decoder ProjectPreview.decoder)


getSettings : AppState -> Uuid -> ToMsg (ProjectDetailWrapper ProjectSettings) msg -> Cmd msg
getSettings appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/settings") (ProjectDetailWrapper.decoder ProjectSettings.decoder)


putSettings : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putSettings appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/settings") body


putShare : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putShare appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/share") body


getUserSuggestions : AppState -> Uuid -> Bool -> String -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUserSuggestions appState projectUuid editor query =
    let
        queryString =
            PaginationQueryString.fromQ query
                |> PaginationQueryString.withSize (Just 10)
                |> PaginationQueryString.toApiUrlWith [ ( "editor", Bool.toString editor ) ]

        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/users/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getMigration : AppState -> Uuid -> ToMsg ProjectMigration msg -> Cmd msg
getMigration appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations/current") ProjectMigration.decoder


getVersions : AppState -> Uuid -> ToMsg (List ProjectVersion) msg -> Cmd msg
getVersions appState uuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/versions") (D.list ProjectVersion.decoder)


getEvents : AppState -> Uuid -> Int -> ToMsg (Pagination ProjectEvent) msg -> Cmd msg
getEvents appState uuid pageNumber =
    Request.get (AppState.toServerInfo appState)
        ("/projects/" ++ Uuid.toString uuid ++ "/events?size=1000&sort=createdAt,desc&page=" ++ String.fromInt pageNumber)
        (Pagination.decoder "projectEvents" ProjectEvent.decoder)


getEvent : AppState -> Uuid -> Uuid -> ToMsg ProjectEvent msg -> Cmd msg
getEvent appState uuid eventUuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/events/" ++ Uuid.toString eventUuid) ProjectEvent.decoder


post : AppState -> Value -> ToMsg Project msg -> Cmd msg
post appState body =
    Request.post (AppState.toServerInfo appState) "/projects" Project.decoder body


postFromTemplate : AppState -> Value -> ToMsg Project msg -> Cmd msg
postFromTemplate appState body =
    Request.post (AppState.toServerInfo appState) "/projects/from-template" Project.decoder body


clone : AppState -> Uuid -> ToMsg Project msg -> Cmd msg
clone appState uuid =
    Request.postEmptyBody (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/clone") Project.decoder


fetchMigration : AppState -> Uuid -> Value -> ToMsg ProjectMigration msg -> Cmd msg
fetchMigration appState uuid body =
    Request.post (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations") ProjectMigration.decoder body


putContent : AppState -> Uuid -> List ProjectEvent -> ToMsg () msg -> Cmd msg
putContent appState uuid events =
    let
        body =
            E.object
                [ ( "events", E.list ProjectEvent.encode events ) ]
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/content") body


putMigration : AppState -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putMigration appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations/current") body


postMigrationCompletion : AppState -> Uuid -> ToMsg () msg -> Cmd msg
postMigrationCompletion appState uuid =
    Request.postEmpty (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations/current/completion")


deleteMigration : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteMigration appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations/current")


delete : AppState -> Uuid -> ToMsg () msg -> Cmd msg
delete appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid)


getSummaryReport : AppState -> Uuid -> ToMsg (ProjectDetailWrapper SummaryReport) msg -> Cmd msg
getSummaryReport appState projectUuid =
    Request.get (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/report") (ProjectDetailWrapper.decoder SummaryReport.decoder)


websocket : AppState -> Uuid -> String
websocket appState projectUuid =
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
                        , Just ("identifier=" ++ Uuid.toString projectUuid)
                        ]

                queryString =
                    String.join "&" queryParams
            in
            webSocketUrl ++ "?" ++ queryString

        Nothing ->
            WebSocket.url serverInfo ("/projects/" ++ Uuid.toString projectUuid ++ "/websocket")


getDocumentPreview : AppState -> Uuid -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getDocumentPreview appState projectUuid toMsg =
    let
        serverInfo =
            AppState.toServerInfo appState
    in
    Http.request
        { method = "GET"
        , headers = Request.authorizationHeaders serverInfo
        , url = serverInfo.apiUrl ++ "/projects/" ++ Uuid.toString projectUuid ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = Request.expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getDocuments : AppState -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments appState projectUuid _ qs =
    let
        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/documents" ++ PaginationQueryString.toApiUrl qs
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documents" Document.decoder)


getFiles : AppState -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectFile) msg -> Cmd msg
getFiles appState projectUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/files" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectFiles" ProjectFile.decoder)


postVersion : AppState -> Uuid -> Value -> ToMsg ProjectVersion msg -> Cmd msg
postVersion appState projectUuid data =
    Request.post (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions") ProjectVersion.decoder data


putVersion : AppState -> Uuid -> Uuid -> Value -> ToMsg ProjectVersion msg -> Cmd msg
putVersion appState projectUuid versionUuid data =
    Request.put (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions/" ++ Uuid.toString versionUuid) ProjectVersion.decoder data


deleteVersion : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteVersion appState projectUuid versionUuid =
    Request.delete (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions/" ++ Uuid.toString versionUuid)


fetchPreview : AppState -> Uuid -> Uuid -> ToMsg ProjectContent msg -> Cmd msg
fetchPreview appState projectUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.post (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/revert/preview") ProjectContent.decoder body


postRevert : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
postRevert appState projectUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/revert") body


getProjectTagsSuggestions : AppState -> PaginationQueryString -> List String -> ToMsg (Pagination String) msg -> Cmd msg
getProjectTagsSuggestions appState qs exclude =
    let
        queryString =
            PaginationQueryString.withSort (Just "projectTag") PaginationQueryString.SortASC qs
                |> PaginationQueryString.toApiUrlWith [ ( "exclude", String.join "," exclude ) ]

        url =
            "/projects/project-tags/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectTags" D.string)
