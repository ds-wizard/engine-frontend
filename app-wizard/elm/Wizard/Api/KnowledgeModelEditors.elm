module Wizard.Api.KnowledgeModelEditors exposing
    ( deleteKnowledgeModelEditor
    , deleteMigration
    , getKnowledgeModelEditor
    , getKnowledgeModelEditorSuggestions
    , getKnowledgeModelEditors
    , getMigration
    , postKnowledgeModelEditor
    , postMigration
    , postMigrationConflict
    , postMigrationConflictApplyAll
    , putKnowledgeModelEditor
    , websocket
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Api.WebSocket as WebSocket
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelEditor as KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Wizard.Api.Models.KnowledgeModelEditorDetail as KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Wizard.Api.Models.KnowledgeModelMigration as Migration exposing (KnowledgeModelMigration)
import Wizard.Data.AppState as AppState exposing (AppState)


getKnowledgeModelEditors : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelEditor) msg -> Cmd msg
getKnowledgeModelEditors appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/knowledge-model-editors" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelEditors" KnowledgeModelEditor.decoder)


getKnowledgeModelEditorSuggestions : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelEditorSuggestion) msg -> Cmd msg
getKnowledgeModelEditorSuggestions appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/knowledge-model-editors/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelEditors" KnowledgeModelEditorSuggestion.decoder)


getKnowledgeModelEditor : AppState -> Uuid -> ToMsg KnowledgeModelEditorDetail msg -> Cmd msg
getKnowledgeModelEditor appState uuid =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid) KnowledgeModelEditorDetail.decoder


postKnowledgeModelEditor : AppState -> E.Value -> ToMsg KnowledgeModelEditor msg -> Cmd msg
postKnowledgeModelEditor appState body =
    Request.post (AppState.toServerInfo appState) "/knowledge-model-editors" KnowledgeModelEditor.decoder body


putKnowledgeModelEditor : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putKnowledgeModelEditor appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid) body


deleteKnowledgeModelEditor : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelEditor appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid)


websocket : AppState -> Uuid -> String
websocket appState uuid =
    WebSocket.url (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/websocket")


getMigration : AppState -> Uuid -> ToMsg KnowledgeModelMigration msg -> Cmd msg
getMigration appState uuid =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current") Migration.decoder


postMigration : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigration appState uuid body =
    Request.postWhatever (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current") body


postMigrationConflict : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigrationConflict appState uuid body =
    Request.postWhatever (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current/conflict") body


postMigrationConflictApplyAll : AppState -> Uuid -> ToMsg () msg -> Cmd msg
postMigrationConflictApplyAll appState uuid =
    Request.postEmpty (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current/conflict/all")


deleteMigration : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteMigration appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current")
