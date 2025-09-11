module Wizard.Api.Branches exposing
    ( deleteBranch
    , deleteMigration
    , getBranch
    , getBranchSuggestions
    , getBranches
    , getMigration
    , postBranch
    , postMigration
    , postMigrationConflict
    , postMigrationConflictApplyAll
    , putBranch
    , websocket
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Api.WebSocket as WebSocket
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Branch as Branch exposing (Branch)
import Wizard.Api.Models.BranchDetail as BranchDetail exposing (BranchDetail)
import Wizard.Api.Models.BranchSuggestion as BranchSuggestion exposing (BranchSuggestion)
import Wizard.Api.Models.Migration as Migration exposing (Migration)
import Wizard.Data.AppState as AppState exposing (AppState)


getBranches : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Branch) msg -> Cmd msg
getBranches appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/branches" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "branches" Branch.decoder)


getBranchSuggestions : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination BranchSuggestion) msg -> Cmd msg
getBranchSuggestions appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/branches/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "branches" BranchSuggestion.decoder)


getBranch : AppState -> Uuid -> ToMsg BranchDetail msg -> Cmd msg
getBranch appState uuid =
    Request.get (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid) BranchDetail.decoder


postBranch : AppState -> E.Value -> ToMsg Branch msg -> Cmd msg
postBranch appState body =
    Request.post (AppState.toServerInfo appState) "/branches" Branch.decoder body


putBranch : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putBranch appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid) body


deleteBranch : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteBranch appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid)


websocket : AppState -> Uuid -> String
websocket appState uuid =
    WebSocket.url (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/websocket")


getMigration : AppState -> Uuid -> ToMsg Migration msg -> Cmd msg
getMigration appState uuid =
    Request.get (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current") Migration.decoder


postMigration : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigration appState uuid body =
    Request.postWhatever (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current") body


postMigrationConflict : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
postMigrationConflict appState uuid body =
    Request.postWhatever (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current/conflict") body


postMigrationConflictApplyAll : AppState -> Uuid -> ToMsg () msg -> Cmd msg
postMigrationConflictApplyAll appState uuid =
    Request.postEmpty (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current/conflict/all")


deleteMigration : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteMigration appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current")
