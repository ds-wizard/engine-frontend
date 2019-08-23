module KMEditor.Migration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Branches as BranchesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lg)
import Common.Setters exposing (setMigration)
import KMEditor.Common.Events.Event as Event
import KMEditor.Common.Migration exposing (Migration)
import KMEditor.Common.MigrationResolution as MigrationResolution exposing (MigrationResolution)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import Msgs


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getMigration uuid appState GetMigrationCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetMigrationCompleted result ->
            handleGetMigrationCompleted appState model result

        ApplyEvent ->
            handleApplyEvent wrapMsg appState model

        RejectEvent ->
            handleRejectEvent wrapMsg appState model

        PostMigrationConflictCompleted result ->
            handlePostMigrationConflictCompleted wrapMsg appState model result



-- Handlers


handleGetMigrationCompleted : AppState -> Model -> Result ApiError Migration -> ( Model, Cmd Msgs.Msg )
handleGetMigrationCompleted appState model result =
    applyResult
        { setResult = setMigration
        , defaultError = lg "apiError.branches.migrations.getError" appState
        , model = model
        , result = result
        }


handleApplyEvent : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleApplyEvent =
    resolveChange MigrationResolution.apply


handleRejectEvent : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleRejectEvent =
    resolveChange MigrationResolution.reject


handlePostMigrationConflictCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePostMigrationConflictCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            let
                cmd =
                    Cmd.map wrapMsg <| fetchData model.branchUuid appState
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = getServerError error <| lg "apiError.branches.migrations.conflict.postError" appState }
            , getResultCmd result
            )



-- Helpers


resolveChange : (String -> MigrationResolution) -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
resolveChange createMigrationResolution wrapMsg appState model =
    let
        cmd =
            case model.migration of
                Success migration ->
                    migration.migrationState.targetEvent
                        |> Maybe.map Event.getUuid
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
            MigrationResolution.encode resolution
    in
    Cmd.map wrapMsg <|
        BranchesApi.postMigrationConflict uuid body appState PostMigrationConflictCompleted
