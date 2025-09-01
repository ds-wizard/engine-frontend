module Wizard.KMEditor.Migration.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setMigration)
import Uuid exposing (Uuid)
import Wizard.Api.Branches as BranchesApi
import Wizard.Api.Models.Event as Event
import Wizard.Api.Models.Migration exposing (Migration)
import Wizard.Api.Models.MigrationResolution as MigrationResolution exposing (MigrationResolution)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Migration.Models exposing (ButtonClicked(..), Model)
import Wizard.KMEditor.Migration.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getMigration appState uuid GetMigrationCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetMigrationCompleted result ->
            handleGetMigrationCompleted appState model result

        ApplyAll ->
            handleApplyAll wrapMsg appState model

        ApplyEvent ->
            handleApplyEvent wrapMsg appState model

        RejectEvent ->
            handleRejectEvent wrapMsg appState model

        PostMigrationConflictCompleted result ->
            handlePostMigrationConflictCompleted wrapMsg appState model result



-- Handlers


handleGetMigrationCompleted : AppState -> Model -> Result ApiError Migration -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetMigrationCompleted appState model result =
    RequestHelpers.applyResult
        { setResult = setMigration
        , defaultError = gettext "Unable to get migration." appState.locale
        , model = model
        , result = result
        , logoutMsg = Wizard.Msgs.logoutMsg
        , locale = appState.locale
        }


handleApplyAll : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleApplyAll wrapMsg appState model =
    let
        cmd =
            Cmd.map wrapMsg <|
                BranchesApi.postMigrationConflictApplyAll appState model.branchUuid PostMigrationConflictCompleted
    in
    ( { model | conflict = Loading, buttonClicked = Just ApplyAllButtonClicked }, cmd )


handleApplyEvent : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleApplyEvent =
    resolveChange MigrationResolution.apply ApplyButtonClicked


handleRejectEvent : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleRejectEvent =
    resolveChange MigrationResolution.reject RejectButtonClicked


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
            ( { model | conflict = ApiError.toActionResult appState (gettext "Unable to resolve conflict." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )



-- Helpers


resolveChange : (String -> MigrationResolution) -> ButtonClicked -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
resolveChange createMigrationResolution buttonClicked wrapMsg appState model =
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
    ( { model | conflict = Loading, buttonClicked = Just buttonClicked }, cmd )


postMigrationConflictCmd : (Msg -> Wizard.Msgs.Msg) -> Uuid -> AppState -> MigrationResolution -> Cmd Wizard.Msgs.Msg
postMigrationConflictCmd wrapMsg uuid appState resolution =
    let
        body =
            MigrationResolution.encode resolution
    in
    Cmd.map wrapMsg <|
        BranchesApi.postMigrationConflict appState uuid body PostMigrationConflictCompleted
