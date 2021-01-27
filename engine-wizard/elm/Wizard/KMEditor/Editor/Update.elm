module Wizard.KMEditor.Editor.Update exposing
    ( fetchData
    , isGuarded
    , update
    )

import ActionResult exposing (ActionResult(..))
import Maybe.Extra exposing (isJust)
import Random exposing (Seed)
import Shared.Api.Branches as BranchesApi
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Levels as LevelsApi
import Shared.Api.Metrics as MetricsApi
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (l, lg)
import Task
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models
import Wizard.KMEditor.Editor.KMEditor.Update exposing (generateEvents)
import Wizard.KMEditor.Editor.Models exposing (EditorType(..), Model, addSessionEvents, containsChanges, getAllEvents, getCurrentActiveEditorUuid, initialModel)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Editor.Preview.Models
import Wizard.KMEditor.Editor.Preview.Update
import Wizard.KMEditor.Editor.TagEditor.Models as TagEditorModel
import Wizard.KMEditor.Editor.TagEditor.Update
import Wizard.Msgs
import Wizard.Ports as Ports


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.Update"


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    Cmd.batch
        [ BranchesApi.getBranch uuid appState GetKnowledgeModelCompleted
        , MetricsApi.getMetrics appState GetMetricsCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    if containsChanges model then
        Just <| l_ "unsavedChanges" appState

    else
        Nothing


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    let
        updateResult =
            case msg of
                GetKnowledgeModelCompleted result ->
                    let
                        ( newModel, cmd ) =
                            case result of
                                Ok km ->
                                    fetchPreview wrapMsg appState { model | km = Success km }

                                Err error ->
                                    ( { model | km = ApiError.toActionResult appState (lg "apiError.branches.getError" appState) error }
                                    , getResultCmd result
                                    )
                    in
                    ( appState.seed, newModel, cmd )

                GetMetricsCompleted result ->
                    let
                        ( newModel, cmd ) =
                            case result of
                                Ok metrics ->
                                    fetchPreview wrapMsg appState { model | metrics = Success metrics }

                                Err error ->
                                    ( { model | metrics = ApiError.toActionResult appState (lg "apiError.metrics.getListError" appState) error }
                                    , getResultCmd result
                                    )
                    in
                    ( appState.seed, newModel, cmd )

                GetLevelsCompleted result ->
                    let
                        ( newModel, cmd ) =
                            case result of
                                Ok levels ->
                                    fetchPreview wrapMsg appState { model | levels = Success levels }

                                Err error ->
                                    ( { model | levels = ApiError.toActionResult appState (lg "apiError.levels.getListError" appState) error }
                                    , getResultCmd result
                                    )
                    in
                    ( appState.seed, newModel, cmd )

                GetPreviewCompleted result ->
                    let
                        newModel =
                            case result of
                                Ok km ->
                                    { model
                                        | preview = Success km
                                        , previewEditorModel =
                                            Just <|
                                                Wizard.KMEditor.Editor.Preview.Models.initialModel appState
                                                    km
                                                    (ActionResult.withDefault [] model.metrics)
                                                    (ActionResult.withDefault [] model.levels)
                                                    (getAllEvents model)
                                                    (ActionResult.withDefault "" <| ActionResult.map (Maybe.withDefault "" << .previousPackageId) model.km)
                                        , tagEditorModel = Just <| TagEditorModel.initialModel km
                                        , editorModel =
                                            Just <|
                                                Wizard.KMEditor.Editor.KMEditor.Models.initialModel
                                                    km
                                                    model.sessionActiveEditor
                                                    (ActionResult.withDefault [] model.metrics)
                                                    (ActionResult.withDefault [] model.levels)
                                                    ((ActionResult.withDefault [] <| ActionResult.map .events model.km) ++ model.sessionEvents)
                                    }

                                Err error ->
                                    { model | preview = ApiError.toActionResult appState (lg "apiError.knowledgeModels.preview.fetchError" appState) error }

                        cmd =
                            getResultCmd result
                    in
                    ( appState.seed, newModel, cmd )

                OpenEditor editor ->
                    let
                        ( newSeed, modelWithEvents ) =
                            applyCurrentEditorChanges appState appState.seed model

                        ( newModel, cmd ) =
                            fetchPreview wrapMsg appState { modelWithEvents | currentEditor = editor, sessionActiveEditor = getCurrentActiveEditorUuid model }
                    in
                    ( newSeed, newModel, cmd )

                PreviewEditorMsg previewMsg ->
                    let
                        ( newSeed, previewEditorModel, cmd ) =
                            case model.previewEditorModel of
                                Just m ->
                                    let
                                        ( newSeed1, newPreviewEditorModel, newCmd ) =
                                            Wizard.KMEditor.Editor.Preview.Update.update previewMsg appState m
                                    in
                                    ( newSeed1
                                    , Just newPreviewEditorModel
                                    , Cmd.map (wrapMsg << PreviewEditorMsg) newCmd
                                    )

                                Nothing ->
                                    ( appState.seed, Nothing, Cmd.none )
                    in
                    ( newSeed, { model | previewEditorModel = previewEditorModel }, cmd )

                TagEditorMsg tagMsg ->
                    let
                        ( newTagEditorModel, cmd ) =
                            case model.tagEditorModel of
                                Just tagEditorModel ->
                                    let
                                        ( updatedTagEditorModel, updateCmd ) =
                                            Wizard.KMEditor.Editor.TagEditor.Update.update tagMsg tagEditorModel
                                    in
                                    ( Just updatedTagEditorModel
                                    , Cmd.map (wrapMsg << TagEditorMsg) updateCmd
                                    )

                                Nothing ->
                                    ( Nothing, Cmd.none )
                    in
                    ( appState.seed, { model | tagEditorModel = newTagEditorModel }, cmd )

                KMEditorMsg editorMsg ->
                    let
                        ( newSeed, newEditorModel, cmd ) =
                            case model.editorModel of
                                Just editorModel ->
                                    let
                                        ( updatedSeed, updatedEditorModel, updateCmd ) =
                                            Wizard.KMEditor.Editor.KMEditor.Update.update editorMsg appState editorModel (openEditorTask wrapMsg)
                                    in
                                    ( updatedSeed, Just updatedEditorModel, updateCmd )

                                Nothing ->
                                    ( appState.seed, Nothing, Cmd.none )
                    in
                    ( newSeed, { model | editorModel = newEditorModel }, cmd )

                Discard ->
                    let
                        ( newModel, cmd ) =
                            fetchPreview wrapMsg appState { model | sessionEvents = [] }
                    in
                    ( appState.seed
                    , newModel
                    , Cmd.batch [ Ports.clearUnloadMessage (), cmd ]
                    )

                Save ->
                    let
                        ( newSeed, newModel ) =
                            applyCurrentEditorChanges appState appState.seed model

                        ( newModel2, cmd ) =
                            if hasKMEditorAlert newModel.editorModel then
                                ( newModel, Cmd.none )

                            else
                                ( { newModel | saving = Loading }
                                , model.km
                                    |> ActionResult.map (putBranchCmd wrapMsg appState newModel)
                                    |> ActionResult.withDefault Cmd.none
                                )
                    in
                    ( newSeed, newModel2, cmd )

                SaveCompleted result ->
                    case result of
                        Ok _ ->
                            let
                                newModel =
                                    initialModel model.kmUuid
                            in
                            ( appState.seed
                            , { newModel | currentEditor = model.currentEditor, sessionActiveEditor = getCurrentActiveEditorUuid model }
                            , Cmd.batch
                                [ Ports.clearUnloadMessage ()
                                , Cmd.map wrapMsg <| fetchData model.kmUuid appState
                                ]
                            )

                        Err error ->
                            ( appState.seed
                            , { model | saving = ApiError.toActionResult appState (lg "apiError.branches.putError" appState) error }
                            , getResultCmd result
                            )
    in
    withSetUnloadMsgCmd appState updateResult


openEditorTask : (Msg -> Wizard.Msgs.Msg) -> Cmd Wizard.Msgs.Msg
openEditorTask wrapMsg =
    Task.perform (wrapMsg << OpenEditor) (Task.succeed KMEditor)


fetchPreview : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
fetchPreview wrapMsg appState model =
    case ActionResult.combine3 model.km model.metrics model.levels of
        Success ( km, _, _ ) ->
            ( { model | preview = Loading }
            , Cmd.map wrapMsg <|
                KnowledgeModelsApi.fetchPreview
                    km.previousPackageId
                    (getAllEvents model)
                    []
                    appState
                    GetPreviewCompleted
            )

        _ ->
            ( model, Cmd.none )


putBranchCmd : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> BranchDetail -> Cmd Wizard.Msgs.Msg
putBranchCmd wrapMsg appState model km =
    Cmd.map wrapMsg <|
        BranchesApi.putBranch
            model.kmUuid
            km.name
            km.kmId
            (km.events ++ model.sessionEvents)
            appState
            SaveCompleted


applyCurrentEditorChanges : AppState -> Seed -> Model -> ( Seed, Model )
applyCurrentEditorChanges appState seed model =
    case ( model.currentEditor, model.preview ) of
        ( TagsEditor, Success km ) ->
            let
                ( newSeed, newEvents ) =
                    model.tagEditorModel
                        |> Maybe.map (TagEditorModel.generateEvents seed km)
                        |> Maybe.withDefault ( seed, [] )
            in
            ( newSeed, addSessionEvents newEvents model )

        ( KMEditor, Success km ) ->
            let
                map ( mapSeed, editorModel, _ ) =
                    ( mapSeed, editorModel.events, Just editorModel )

                ( newSeed, newEvents, newEditorModel ) =
                    model.editorModel
                        |> Maybe.map (map << generateEvents appState seed)
                        |> Maybe.withDefault ( seed, [], model.editorModel )
            in
            if hasKMEditorAlert newEditorModel then
                ( newSeed, { model | editorModel = newEditorModel } )

            else
                ( newSeed, addSessionEvents newEvents model )

        _ ->
            ( seed, model )


hasKMEditorAlert : Maybe Wizard.KMEditor.Editor.KMEditor.Models.Model -> Bool
hasKMEditorAlert =
    Maybe.map (.alert >> isJust) >> Maybe.withDefault False


withSetUnloadMsgCmd : AppState -> ( a, Model, Cmd msg ) -> ( a, Model, Cmd msg )
withSetUnloadMsgCmd appState ( a, model, cmd ) =
    let
        newCmd =
            if containsChanges model then
                Cmd.batch [ cmd, Ports.setUnloadMessage <| l_ "unsavedChanges" appState ]

            else
                cmd
    in
    ( a, model, newCmd )
