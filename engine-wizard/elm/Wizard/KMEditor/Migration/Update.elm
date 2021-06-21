module Wizard.KMEditor.Migration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Branches as BranchesApi
import Shared.Data.Event as Event
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Migration exposing (Migration)
import Shared.Data.MigrationResolution as MigrationResolution exposing (MigrationResolution)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setMetrics, setMigration)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Migration.Models exposing (Model)
import Wizard.KMEditor.Migration.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getMigration uuid appState GetMigrationCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleGetMigrationCompleted : AppState -> Model -> Result ApiError Migration -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetMigrationCompleted appState model result =
    applyResult appState
        { setResult = setMigration
        , defaultError = lg "apiError.branches.migrations.getError" appState
        , model = model
        , result = result
        }


handleApplyEvent : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleApplyEvent =
    resolveChange MigrationResolution.apply


handleRejectEvent : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleRejectEvent =
    resolveChange MigrationResolution.reject


handlePostMigrationConflictCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostMigrationConflictCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            let
                cmd =
                    Cmd.map wrapMsg <| fetchData model.branchUuid appState
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = ApiError.toActionResult appState (lg "apiError.branches.migrations.conflict.postError" appState) error }
            , getResultCmd result
            )



-- Helpers


resolveChange : (String -> MigrationResolution) -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


postMigrationConflictCmd : (Msg -> Wizard.Msgs.Msg) -> Uuid -> AppState -> MigrationResolution -> Cmd Wizard.Msgs.Msg
postMigrationConflictCmd wrapMsg uuid appState resolution =
    let
        body =
            MigrationResolution.encode resolution
    in
    Cmd.map wrapMsg <|
        BranchesApi.postMigrationConflict uuid body appState PostMigrationConflictCompleted
