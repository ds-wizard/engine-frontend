module KMEditor.Migration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import KMEditor.Common.Models.Events exposing (getEventUuid)
import KMEditor.Common.Models.Migration exposing (Migration, MigrationResolution, encodeMigrationResolution, newApplyMigrationResolution, newRejectMigrationResolution)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import Msgs


fetchData : (Msg -> Msgs.Msg) -> String -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg uuid appState =
    Cmd.map wrapMsg <|
        KnowledgeModelsApi.getMigration uuid appState GetMigrationCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetMigrationCompleted result ->
            handleGetMigrationCompleted model result

        ApplyEvent ->
            handleAcceptChange wrapMsg appState model

        RejectEvent ->
            handleRejectChange wrapMsg appState model

        PostMigrationConflictCompleted result ->
            handlePostMigrationConflictCompleted wrapMsg appState model result


handleGetMigrationCompleted : Model -> Result ApiError Migration -> ( Model, Cmd Msgs.Msg )
handleGetMigrationCompleted model result =
    case result of
        Ok migration ->
            ( { model | migration = Success migration }, Cmd.none )

        Err error ->
            ( { model | migration = getServerError error "Unable to get migration" }
            , getResultCmd result
            )


handleAcceptChange : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleAcceptChange =
    handleResolveChange newApplyMigrationResolution


handleRejectChange : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleRejectChange =
    handleResolveChange newRejectMigrationResolution


handleResolveChange : (String -> MigrationResolution) -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleResolveChange createMigrationResolution wrapMsg appState model =
    let
        cmd =
            case model.migration of
                Success migration ->
                    migration.migrationState.targetEvent
                        |> Maybe.map getEventUuid
                        |> Maybe.map createMigrationResolution
                        |> Maybe.map (postMigrationConflictCmd wrapMsg model.branchUuid appState)
                        |> Maybe.withDefault Cmd.none

                _ ->
                    Cmd.none
    in
    ( { model | conflict = Loading }, cmd )


postMigrationConflictCmd : (Msg -> Msgs.Msg) -> String -> AppState -> MigrationResolution -> Cmd Msgs.Msg
postMigrationConflictCmd wrapMsg uuid appState resolution =
    let
        body =
            encodeMigrationResolution resolution
    in
    Cmd.map wrapMsg <|
        KnowledgeModelsApi.postMigrationConflict uuid body appState PostMigrationConflictCompleted


handlePostMigrationConflictCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePostMigrationConflictCompleted wrapMsg appState model result =
    case result of
        Ok migration ->
            let
                cmd =
                    fetchData wrapMsg model.branchUuid appState
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = getServerError error "Unable to resolve conflict" }
            , getResultCmd result
            )
