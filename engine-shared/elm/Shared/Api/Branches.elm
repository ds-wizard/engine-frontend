module Shared.Api.Branches exposing
    ( deleteBranch
    , deleteMigration
    , getBranch
    , getBranches
    , getMigration
    , postBranch
    , postMigration
    , postMigrationConflict
    , postMigrationConflictApplyAll
    , putBranch
    , websocket
    )

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPostEmpty, jwtPut, wsUrl)
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.BranchDetail as BranchDetail exposing (BranchDetail)
import Shared.Data.Migration as Migration exposing (Migration)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


getBranches : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Branch) msg -> Cmd msg
getBranches qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/branches" ++ queryString
    in
    jwtGet url (Pagination.decoder "branches" Branch.decoder)


getBranch : Uuid -> AbstractAppState a -> ToMsg BranchDetail msg -> Cmd msg
getBranch uuid =
    jwtGet ("/branches/" ++ Uuid.toString uuid) BranchDetail.decoder


postBranch : E.Value -> AbstractAppState a -> ToMsg Branch msg -> Cmd msg
postBranch =
    jwtFetch "/branches" Branch.decoder


putBranch : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putBranch uuid body =
    jwtPut ("/branches/" ++ Uuid.toString uuid) body


deleteBranch : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteBranch uuid =
    jwtDelete ("/branches/" ++ Uuid.toString uuid)


websocket : Uuid -> AbstractAppState a -> String
websocket uuid =
    wsUrl ("/branches/" ++ Uuid.toString uuid ++ "/websocket")


getMigration : Uuid -> AbstractAppState a -> ToMsg Migration msg -> Cmd msg
getMigration uuid =
    jwtGet ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current") Migration.decoder


postMigration : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postMigration uuid =
    jwtPost ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current")


postMigrationConflict : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postMigrationConflict uuid =
    jwtPost ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current/conflict")


postMigrationConflictApplyAll : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postMigrationConflictApplyAll uuid =
    jwtPostEmpty ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current/conflict/all")


deleteMigration : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteMigration uuid =
    jwtDelete ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current")
