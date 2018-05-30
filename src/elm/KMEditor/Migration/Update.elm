module KMEditor.Migration.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMEditor.Common.Models.Events exposing (getEventUuid)
import KMEditor.Common.Models.Migration exposing (Migration, MigrationResolution, encodeMigrationResolution, newApplyMigrationResolution, newRejectMigrationResolution)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (getMigration, postMigrationConflict)
import Msgs


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    getMigration uuid session
        |> Jwt.send GetMigrationCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetMigrationCompleted result ->
            handleGetMigrationCompleted model result

        ApplyEvent ->
            handleAcceptChange wrapMsg session model

        RejectEvent ->
            handleRejectChange wrapMsg session model

        PostMigrationConflictCompleted result ->
            handlePostMigrationConflictCompleted wrapMsg session model result


handleGetMigrationCompleted : Model -> Result Jwt.JwtError Migration -> ( Model, Cmd Msgs.Msg )
handleGetMigrationCompleted model result =
    case result of
        Ok migration ->
            ( { model | migration = Success migration }, Cmd.none )

        Err error ->
            ( { model | migration = getServerErrorJwt error "Unable to get migration" }, Cmd.none )


handleAcceptChange : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleAcceptChange =
    handleResolveChange newApplyMigrationResolution


handleRejectChange : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleRejectChange =
    handleResolveChange newRejectMigrationResolution


handleResolveChange : (String -> MigrationResolution) -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleResolveChange createMigrationResolution wrapMsg session model =
    let
        cmd =
            case model.migration of
                Success migration ->
                    migration.migrationState.targetEvent
                        |> Maybe.map getEventUuid
                        |> Maybe.map createMigrationResolution
                        |> Maybe.map (postMigrationConflictCmd wrapMsg model.branchUuid session)
                        |> Maybe.withDefault Cmd.none

                _ ->
                    Cmd.none
    in
    ( { model | conflict = Loading }, cmd )


postMigrationConflictCmd : (Msg -> Msgs.Msg) -> String -> Session -> MigrationResolution -> Cmd Msgs.Msg
postMigrationConflictCmd wrapMsg uuid session resolution =
    resolution
        |> encodeMigrationResolution
        |> postMigrationConflict uuid session
        |> Jwt.send PostMigrationConflictCompleted
        |> Cmd.map wrapMsg


handlePostMigrationConflictCompleted : (Msg -> Msgs.Msg) -> Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
handlePostMigrationConflictCompleted wrapMsg session model result =
    case result of
        Ok migration ->
            let
                cmd =
                    fetchData wrapMsg model.branchUuid session
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = getServerErrorJwt error "Unable to resolve conflict" }, Cmd.none )
