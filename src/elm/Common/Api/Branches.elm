module Common.Api.Branches exposing
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

import Common.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Common.AppState exposing (AppState)
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Branch as Branch exposing (Branch)
import KMEditor.Common.BranchDetail as BranchDetail exposing (BranchDetail)
import KMEditor.Common.Events.Event as Event exposing (Event)
import KMEditor.Common.Migration as Migration exposing (Migration)


getBranches : AppState -> ToMsg (List Branch) msg -> Cmd msg
getBranches =
    jwtGet "/branches" Branch.listDecoder


getBranch : String -> AppState -> ToMsg BranchDetail msg -> Cmd msg
getBranch uuid =
    jwtGet ("/branches/" ++ uuid) BranchDetail.decoder


postBranch : Value -> AppState -> ToMsg Branch msg -> Cmd msg
postBranch =
    jwtFetch "/branches" Branch.decoder


putBranch : String -> String -> String -> List Event -> AppState -> ToMsg () msg -> Cmd msg
putBranch uuid name kmId events =
    let
        body =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "kmId", Encode.string kmId )
                , ( "events", Encode.list Event.encode events )
                ]
    in
    jwtPut ("/branches/" ++ uuid) body


deleteBranch : String -> AppState -> ToMsg () msg -> Cmd msg
deleteBranch uuid =
    jwtDelete ("/branches/" ++ uuid)


putVersion : String -> String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putVersion kmUuid version =
    jwtPut ("/branches/" ++ kmUuid ++ "/versions/" ++ version)


getMigration : String -> AppState -> ToMsg Migration msg -> Cmd msg
getMigration uuid =
    jwtGet ("/branches/" ++ uuid ++ "/migrations/current") Migration.decoder


postMigration : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
postMigration uuid =
    jwtPost ("/branches/" ++ uuid ++ "/migrations/current")


postMigrationConflict : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
postMigrationConflict uuid =
    jwtPost ("/branches/" ++ uuid ++ "/migrations/current/conflict")


deleteMigration : String -> AppState -> ToMsg () msg -> Cmd msg
deleteMigration uuid =
    jwtDelete ("/branches/" ++ uuid ++ "/migrations/current")
