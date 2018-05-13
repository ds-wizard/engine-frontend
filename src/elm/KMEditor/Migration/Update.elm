module KMEditor.Migration.Update exposing (getMigrationCmd, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMEditor.Editor.Models.Events exposing (getEventUuid)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import KMEditor.Models.Migration exposing (..)
import KMEditor.Requests exposing (getMigration, postMigrationConflict)
import Msgs
import Requests exposing (toCmd)


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetMigrationCompleted result ->
            handleGetMigrationCompleted model result

        ApplyEvent ->
            handleAcceptChange session model

        RejectEvent ->
            handleRejectChange session model

        PostMigrationConflictCompleted result ->
            handlePostMigrationConflictCompleted session model result


getMigrationCmd : String -> Session -> Cmd Msgs.Msg
getMigrationCmd uuid session =
    getMigration uuid session
        |> toCmd GetMigrationCompleted Msgs.KMEditorMigrationMsg


postMigrationConflictCmd : String -> Session -> MigrationResolution -> Cmd Msgs.Msg
postMigrationConflictCmd uuid session resolution =
    resolution
        |> encodeMigrationResolution
        |> postMigrationConflict uuid session
        |> toCmd PostMigrationConflictCompleted Msgs.KMEditorMigrationMsg


handleGetMigrationCompleted : Model -> Result Jwt.JwtError Migration -> ( Model, Cmd Msgs.Msg )
handleGetMigrationCompleted model result =
    case result of
        Ok migration ->
            ( { model | migration = Success migration }, Cmd.none )

        Err error ->
            ( { model | migration = getServerErrorJwt error "Unable to get migration" }, Cmd.none )


handleResolveChange : (String -> MigrationResolution) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleResolveChange createMigrationResolution session model =
    let
        cmd =
            case model.migration of
                Success migration ->
                    migration.migrationState.targetEvent
                        |> Maybe.map getEventUuid
                        |> Maybe.map createMigrationResolution
                        |> Maybe.map (postMigrationConflictCmd model.branchUuid session)
                        |> Maybe.withDefault Cmd.none

                _ ->
                    Cmd.none
    in
    ( { model | conflict = Loading }, cmd )


handleAcceptChange : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleAcceptChange =
    handleResolveChange newApplyMigrationResolution


handleRejectChange : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleRejectChange =
    handleResolveChange newRejectMigrationResolution


handlePostMigrationConflictCompleted : Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
handlePostMigrationConflictCompleted session model result =
    case result of
        Ok migration ->
            let
                cmd =
                    getMigrationCmd model.branchUuid session
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = getServerErrorJwt error "Unable to resolve conflict" }, Cmd.none )
