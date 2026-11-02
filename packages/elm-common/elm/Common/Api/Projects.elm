module Common.Api.Projects exposing
    ( clone
    , delete
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
    , postRevert
    , postVersion
    , putContent
    , putSettings
    , putShare
    , putVersion
    , websocket
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Document as Document exposing (Document)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.Project as Project exposing (Project)
import Common.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectContent as ProjectContent exposing (ProjectContent)
import Common.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetailWrapper as ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectFile as ProjectFile exposing (ProjectFile)
import Common.Api.Models.ProjectPreview as ProjectPreview exposing (ProjectPreview)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Models.ProjectSettings as ProjectSettings exposing (ProjectSettings)
import Common.Api.Models.ProjectSuggestion as ProjectSuggestion exposing (ProjectSuggestion)
import Common.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)
import Common.Api.Models.SummaryReport as SummaryReport exposing (SummaryReport)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Api.WebSocket as WebSocket exposing (WebSocket)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Uuid exposing (Uuid)


getList : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Project) msg -> Cmd msg
getList appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            Params.apply appState "GET" ("/projects" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "projects" Project.decoder)


getSuggestions : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectSuggestion) msg -> Cmd msg
getSuggestions appState filters qs =
    let
        extraParams =
            createListExtraParams filters

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            Params.apply appState "GET" ("/projects" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "projects" ProjectSuggestion.decoder)


createListExtraParams : PaginationQueryFilters -> List ( String, String )
createListExtraParams filters =
    PaginationQueryString.filterParams
        [ ( "isTemplate", PaginationQueryFilters.getValue "isTemplate" filters )
        , ( "userUuids", PaginationQueryFilters.getValue "userUuids" filters )
        , ( "userUuidsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "userUuids" filters) )
        , ( "userGroupUuids", PaginationQueryFilters.getValue "userGroupUuids" filters )
        , ( "userGroupUuidsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "userGroupUuids" filters) )
        , ( "projectTags", PaginationQueryFilters.getValue "projectTags" filters )
        , ( "projectTagsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "projectTags" filters) )
        , ( "knowledgeModelPackageIds", PaginationQueryFilters.getValue "knowledgeModelPackages" filters )
        , ( "knowledgeModelPackageIdsOp", Maybe.map FilterOperator.toString (PaginationQueryFilters.getOp "knowledgeModelPackages" filters) )
        ]


get : Context ctx -> Uuid -> ToMsg ProjectCommon msg -> Cmd msg
get appState uuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid) ProjectCommon.decoder


getQuestionnaire : Context ctx -> Uuid -> ToMsg (ProjectDetailWrapper ProjectQuestionnaire) msg -> Cmd msg
getQuestionnaire appState uuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/questionnaire") (ProjectDetailWrapper.decoder ProjectQuestionnaire.decoder)


getCommentThreads : Context ctx -> Uuid -> String -> ToMsg (Dict String (List CommentThread)) msg -> Cmd msg
getCommentThreads appState projectUuid path =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/comments?path=" ++ path) (D.dict (D.list CommentThread.decoder))


getPreview : Context ctx -> Uuid -> ToMsg (ProjectDetailWrapper ProjectPreview) msg -> Cmd msg
getPreview appState uuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/preview") (ProjectDetailWrapper.decoder ProjectPreview.decoder)


getSettings : Context ctx -> Uuid -> ToMsg (ProjectDetailWrapper ProjectSettings) msg -> Cmd msg
getSettings appState uuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/settings") (ProjectDetailWrapper.decoder ProjectSettings.decoder)


putSettings : Context ctx -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putSettings appState uuid body =
    Request.putWhatever (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/settings") body


putShare : Context ctx -> Uuid -> Value -> ToMsg () msg -> Cmd msg
putShare appState uuid body =
    Request.putWhatever (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/share") body


getUserSuggestions : Context ctx -> Uuid -> Bool -> String -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUserSuggestions appState projectUuid editor query =
    let
        queryString =
            PaginationQueryString.fromQ query
                |> PaginationQueryString.withSize (Just 10)
                |> PaginationQueryString.toApiUrlWith [ ( "editor", Bool.toString editor ) ]

        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/users/suggestions" ++ queryString
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getVersions : Context ctx -> Uuid -> ToMsg (List ProjectVersion) msg -> Cmd msg
getVersions appState uuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/versions") (D.list ProjectVersion.decoder)


getEvents : Context ctx -> Uuid -> Int -> ToMsg (Pagination ProjectEvent) msg -> Cmd msg
getEvents appState uuid pageNumber =
    Request.get (Context.toServerInfo appState)
        ("/projects/" ++ Uuid.toString uuid ++ "/events?size=1000&sort=createdAt,desc&page=" ++ String.fromInt pageNumber)
        (Pagination.decoder "projectEvents" ProjectEvent.decoder)


getEvent : Context ctx -> Uuid -> Uuid -> ToMsg ProjectEvent msg -> Cmd msg
getEvent appState uuid eventUuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/events/" ++ Uuid.toString eventUuid) ProjectEvent.decoder


post : Context ctx -> Value -> ToMsg Project msg -> Cmd msg
post appState body =
    Request.post (Context.toServerInfo appState) (Params.apply appState "POST" "/projects") Project.decoder body


postFromTemplate : Context ctx -> Value -> ToMsg Project msg -> Cmd msg
postFromTemplate appState body =
    Request.post (Context.toServerInfo appState) (Params.apply appState "POST" "/projects/from-template") Project.decoder body


clone : Context ctx -> Uuid -> ToMsg Project msg -> Cmd msg
clone appState uuid =
    Request.postEmptyBody (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/clone") Project.decoder


fetchMigration : Context ctx -> Uuid -> Value -> ToMsg ProjectCommon msg -> Cmd msg
fetchMigration appState uuid body =
    Request.post (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/migrations") ProjectCommon.decoder body


putContent : Context ctx -> Uuid -> List ProjectEvent -> ToMsg () msg -> Cmd msg
putContent appState uuid events =
    let
        body =
            E.object
                [ ( "events", E.list ProjectEvent.encode events ) ]
    in
    Request.putWhatever (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid ++ "/content") body


delete : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
delete appState uuid =
    Request.delete (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString uuid)


getSummaryReport : Context ctx -> Uuid -> ToMsg (ProjectDetailWrapper SummaryReport) msg -> Cmd msg
getSummaryReport appState projectUuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/report") (ProjectDetailWrapper.decoder SummaryReport.decoder)


websocket : Uuid -> WebSocket
websocket projectUuid =
    WebSocket.init ("/projects/" ++ Uuid.toString projectUuid ++ "/websocket")


getDocumentPreview : Context ctx -> Uuid -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getDocumentPreview appState projectUuid toMsg =
    let
        serverInfo =
            Context.toServerInfo appState
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


getDocuments : Context ctx -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments appState projectUuid _ qs =
    let
        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/documents" ++ PaginationQueryString.toApiUrl qs
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documents" Document.decoder)


getFiles : Context ctx -> Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectFile) msg -> Cmd msg
getFiles appState projectUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/files" ++ queryString
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "projectFiles" ProjectFile.decoder)


postVersion : Context ctx -> Uuid -> Value -> ToMsg ProjectVersion msg -> Cmd msg
postVersion appState projectUuid data =
    Request.post (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions") ProjectVersion.decoder data


putVersion : Context ctx -> Uuid -> Uuid -> Value -> ToMsg ProjectVersion msg -> Cmd msg
putVersion appState projectUuid versionUuid data =
    Request.put (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions/" ++ Uuid.toString versionUuid) ProjectVersion.decoder data


deleteVersion : Context ctx -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteVersion appState projectUuid versionUuid =
    Request.delete (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/versions/" ++ Uuid.toString versionUuid)


fetchPreview : Context ctx -> Uuid -> Uuid -> ToMsg ProjectContent msg -> Cmd msg
fetchPreview appState projectUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.post (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/revert/preview") ProjectContent.decoder body


postRevert : Context ctx -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
postRevert appState projectUuid eventUuid =
    let
        body =
            E.object
                [ ( "eventUuid", Uuid.encode eventUuid ) ]
    in
    Request.postWhatever (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/revert") body


getProjectTagsSuggestions : Context ctx -> PaginationQueryString -> List String -> ToMsg (Pagination String) msg -> Cmd msg
getProjectTagsSuggestions appState qs exclude =
    let
        queryString =
            PaginationQueryString.withSort (Just "projectTag") PaginationQueryString.SortASC qs
                |> PaginationQueryString.toApiUrlWith [ ( "exclude", String.join "," exclude ) ]

        url =
            Params.apply appState "GET" ("/projects/project-tags/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "projectTags" D.string)
