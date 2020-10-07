module Shared.Api.Branches exposing
    ( deleteBranch
    , deleteMigration
    , getBranch
    , getBranches
    , getMigration
    , postBranch
    , postMigration
    , postMigrationConflict
    , putBranch
    , putVersion
    )

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.BranchDetail as BranchDetail exposing (BranchDetail)
import Shared.Data.Event as Event exposing (Event)
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
            "/branches/page" ++ queryString
    in
    jwtGet url (Pagination.decoder "branches" Branch.decoder)


getBranch : Uuid -> AbstractAppState a -> ToMsg BranchDetail msg -> Cmd msg
getBranch uuid =
    jwtGet ("/branches/" ++ Uuid.toString uuid) BranchDetail.decoder


postBranch : E.Value -> AbstractAppState a -> ToMsg Branch msg -> Cmd msg
postBranch =
    jwtFetch "/branches" Branch.decoder


putBranch : Uuid -> String -> String -> List Event -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putBranch uuid name kmId events =
    let
        body =
            E.object
                [ ( "name", E.string name )
                , ( "kmId", E.string kmId )
                , ( "events", E.list Event.encode events )
                ]
    in
    jwtPut ("/branches/" ++ Uuid.toString uuid) body


deleteBranch : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteBranch uuid =
    jwtDelete ("/branches/" ++ Uuid.toString uuid)


putVersion : Uuid -> String -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putVersion kmUuid version =
    jwtPut ("/branches/" ++ Uuid.toString kmUuid ++ "/versions/" ++ version)


getMigration : Uuid -> AbstractAppState a -> ToMsg Migration msg -> Cmd msg
getMigration uuid =
    jwtGet ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current") Migration.decoder


postMigration : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postMigration uuid =
    jwtPost ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current")


postMigrationConflict : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postMigrationConflict uuid =
    jwtPost ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current/conflict")


deleteMigration : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteMigration uuid =
    jwtDelete ("/branches/" ++ Uuid.toString uuid ++ "/migrations/current")
