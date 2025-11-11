module Wizard.Pages.KMEditor.Migration.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setMigration)
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.Event as Event
import Wizard.Api.Models.KnowledgeModelMigration exposing (KnowledgeModelMigration)
import Wizard.Api.Models.KnowledgeModelMigration.KnowledgeModelMigrationState as KnowledgeModelMigrationState
import Wizard.Api.Models.KnowledgeModelMigrationResolution as MigrationResolution exposing (KnowledgeModelMigrationResolution)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Migration.Models exposing (ButtonClicked(..), Model)
import Wizard.Pages.KMEditor.Migration.Msgs exposing (Msg(..))


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    KnowledgeModelEditorsApi.getMigration appState uuid GetMigrationCompleted


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


handleGetMigrationCompleted : AppState -> Model -> Result ApiError KnowledgeModelMigration -> ( Model, Cmd Wizard.Msgs.Msg )
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
                KnowledgeModelEditorsApi.postMigrationConflictApplyAll appState model.kmEditorUuid PostMigrationConflictCompleted
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
                    Cmd.map wrapMsg <| fetchData model.kmEditorUuid appState
            in
            ( { model | migration = Loading, conflict = Unset }, cmd )

        Err error ->
            ( { model | conflict = ApiError.toActionResult appState (gettext "Unable to resolve conflict." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )



-- Helpers


resolveChange : (String -> KnowledgeModelMigrationResolution) -> ButtonClicked -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
resolveChange createMigrationResolution buttonClicked wrapMsg appState model =
    let
        cmd =
            case model.migration of
                Success migration ->
                    case migration.state of
                        KnowledgeModelMigrationState.Conflict targetEvent ->
                            targetEvent
                                |> Maybe.map Event.getUuid
                                |> Maybe.map createMigrationResolution
                                |> Maybe.map (postMigrationConflictCmd wrapMsg model.kmEditorUuid appState)
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    ( { model | conflict = Loading, buttonClicked = Just buttonClicked }, cmd )


postMigrationConflictCmd : (Msg -> Wizard.Msgs.Msg) -> Uuid -> AppState -> KnowledgeModelMigrationResolution -> Cmd Wizard.Msgs.Msg
postMigrationConflictCmd wrapMsg uuid appState resolution =
    let
        body =
            MigrationResolution.encode resolution
    in
    Cmd.map wrapMsg <|
        KnowledgeModelEditorsApi.postMigrationConflict appState uuid body PostMigrationConflictCompleted
