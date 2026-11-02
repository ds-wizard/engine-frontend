module Common.Api.KnowledgeModelEditors exposing
    ( deleteKnowledgeModelEditor
    , deleteMigration
    , getKnowledgeModelEditor
    , getKnowledgeModelEditorLocales
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

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.KnowledgeModelEditor as KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Common.Api.Models.KnowledgeModelEditorDetail as KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Api.Models.KnowledgeModelEditorSuggestion as KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Common.Api.Models.KnowledgeModelLocale as KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Api.Models.KnowledgeModelMigration as Migration exposing (KnowledgeModelMigration)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Api.WebSocket as WebSocket exposing (WebSocket)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


getKnowledgeModelEditors : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelEditor) msg -> Cmd msg
getKnowledgeModelEditors appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            Params.apply appState "GET" ("/knowledge-model-editors" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelEditors" KnowledgeModelEditor.decoder)


getKnowledgeModelEditorSuggestions : Context ctx -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelEditorSuggestion) msg -> Cmd msg
getKnowledgeModelEditorSuggestions appState qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            Params.apply appState "GET" ("/knowledge-model-editors/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelEditors" KnowledgeModelEditorSuggestion.decoder)


getKnowledgeModelEditor : Context ctx -> Uuid -> ToMsg KnowledgeModelEditorDetail msg -> Cmd msg
getKnowledgeModelEditor appState uuid =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid) KnowledgeModelEditorDetail.decoder


getKnowledgeModelEditorLocales : Context ctx -> Uuid -> ToMsg (List KnowledgeModelLocale) msg -> Cmd msg
getKnowledgeModelEditorLocales appState uuid =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/locales") (D.list KnowledgeModelLocale.decoder)


postKnowledgeModelEditor : Context ctx -> E.Value -> ToMsg UuidResponse msg -> Cmd msg
postKnowledgeModelEditor appState body =
    Request.post (Context.toServerInfo appState) (Params.apply appState "POST" "/knowledge-model-editors") UuidResponse.decoder body


putKnowledgeModelEditor : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putKnowledgeModelEditor appState uuid body =
    Request.putWhatever (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid) body


deleteKnowledgeModelEditor : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelEditor appState uuid =
    Request.delete (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid)


websocket : Uuid -> WebSocket
websocket uuid =
    WebSocket.init ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/websocket")


getMigration : Context ctx -> Uuid -> ToMsg KnowledgeModelMigration msg -> Cmd msg
getMigration appState uuid =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current") Migration.decoder


postMigration : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigration appState uuid body =
    Request.postWhatever (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current") body


postMigrationConflict : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigrationConflict appState uuid body =
    Request.postWhatever (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current/conflict") body


postMigrationConflictApplyAll : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
postMigrationConflictApplyAll appState uuid =
    Request.postEmpty (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current/conflict/all")


deleteMigration : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteMigration appState uuid =
    Request.delete (Context.toServerInfo appState) ("/knowledge-model-editors/" ++ Uuid.toString uuid ++ "/migrations/current")
