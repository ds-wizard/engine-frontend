module Wizard.KMEditor.Editor.KMEditor.Models exposing
    ( Model
    , containsChanges
    , getActiveEditor
    , getCurrentIntegrations
    , getCurrentTags
    , getEditorContext
    , getKMEditor
    , initialModel
    , insertEditor
    , setAlert
    )

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Reorderable
import Shared.Data.Event as Event exposing (Event(..))
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Utils exposing (listFilterJust)
import SplitPane exposing (Orientation(..), configureSplitter, percentage)
import Uuid
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal
import Wizard.KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), EditorState(..), KMEditorData, createKnowledgeModelEditor, getEditorUuid, getNewState, isEditorDirty)


type alias Model =
    { kmUuid : String
    , knowledgeModel : KnowledgeModel
    , metrics : List Metric
    , levels : List Level
    , activeEditorUuid : Maybe String
    , editors : Dict String Editor
    , reorderableState : Reorderable.State
    , events : List Event
    , alert : Maybe String
    , splitPane : SplitPane.State
    , moveModal : MoveModal.Model
    }


initialModel : KnowledgeModel -> Maybe String -> List Metric -> List Level -> List Event -> Model
initialModel knowledgeModel mbActiveEditorUuid metrics levels =
    let
        activeEditorUuid =
            Maybe.or mbActiveEditorUuid (Just (Uuid.toString knowledgeModel.uuid))
    in
    createEditors mbActiveEditorUuid
        { kmUuid = Uuid.toString knowledgeModel.uuid
        , knowledgeModel = knowledgeModel
        , metrics = metrics
        , levels = levels
        , activeEditorUuid = activeEditorUuid
        , editors = Dict.fromList []
        , reorderableState = Reorderable.initialState
        , events = []
        , alert = Nothing
        , splitPane = SplitPane.init Horizontal |> configureSplitter (percentage 0.2 (Just ( 0.05, 0.7 )))
        , moveModal = MoveModal.initialModel (Uuid.toString knowledgeModel.uuid)
        }


createEditors : Maybe String -> Model -> List Event -> Model
createEditors mbActiveEditorUuid model events =
    { model
        | editors =
            createKnowledgeModelEditor
                (getEditorContext model)
                mbActiveEditorUuid
                (getEditorState (createEditorStateDict events))
                model.knowledgeModel
                model.editors
    }


getEditorState : Dict String EditorState -> String -> EditorState
getEditorState editorStateDict uuid =
    Dict.get uuid editorStateDict
        |> Maybe.withDefault Initial


createEditorStateDict : List Event -> Dict String EditorState
createEditorStateDict events =
    List.foldl
        (\event dict ->
            let
                entityUuid =
                    Event.getEntityUuid event

                eventState =
                    eventToEditorState event

                currentState =
                    Dict.get entityUuid dict
                        |> Maybe.withDefault Initial
            in
            Dict.insert entityUuid (getNewState currentState eventState) dict
        )
        Dict.empty
        events


eventToEditorState : Event -> EditorState
eventToEditorState event =
    case event of
        AddKnowledgeModelEvent _ _ ->
            AddedEdited

        EditKnowledgeModelEvent _ _ ->
            Edited

        AddChapterEvent _ _ ->
            AddedEdited

        EditChapterEvent _ _ ->
            Edited

        DeleteChapterEvent _ ->
            Deleted

        AddTagEvent _ _ ->
            AddedEdited

        EditTagEvent _ _ ->
            Edited

        DeleteTagEvent _ ->
            Deleted

        AddIntegrationEvent _ _ ->
            AddedEdited

        EditIntegrationEvent _ _ ->
            Edited

        DeleteIntegrationEvent _ ->
            Deleted

        AddQuestionEvent _ _ ->
            AddedEdited

        EditQuestionEvent _ _ ->
            Edited

        DeleteQuestionEvent _ ->
            Deleted

        AddAnswerEvent _ _ ->
            AddedEdited

        EditAnswerEvent _ _ ->
            Edited

        DeleteAnswerEvent _ ->
            Deleted

        AddReferenceEvent _ _ ->
            AddedEdited

        EditReferenceEvent _ _ ->
            Edited

        DeleteReferenceEvent _ ->
            Deleted

        AddExpertEvent _ _ ->
            AddedEdited

        EditExpertEvent _ _ ->
            Edited

        DeleteExpertEvent _ ->
            Deleted

        MoveQuestionEvent _ _ ->
            Edited

        MoveAnswerEvent _ _ ->
            Edited

        MoveReferenceEvent _ _ ->
            Edited

        MoveExpertEvent _ _ ->
            Edited



{- Model helpers -}


getActiveEditor : Model -> Maybe Editor
getActiveEditor model =
    case model.activeEditorUuid of
        Just uuid ->
            Dict.get uuid model.editors

        Nothing ->
            Nothing


getKMEditor : Model -> Maybe Editor
getKMEditor model =
    Dict.get model.kmUuid model.editors


getCurrentTags : Model -> List Tag
getCurrentTags model =
    getKMEditor model
        |> Maybe.map (getTagEditorsUuids >> getTags model)
        |> Maybe.withDefault []


getCurrentIntegrations : Model -> List Integration
getCurrentIntegrations model =
    getKMEditor model
        |> Maybe.map (getIntegrationEditorsUuids >> getIntegrations model)
        |> Maybe.withDefault []


getTagEditorsUuids : Editor -> List String
getTagEditorsUuids editor =
    case editor of
        KMEditor kmEditorData ->
            kmEditorData.tags.list

        _ ->
            []


getIntegrationEditorsUuids : Editor -> List String
getIntegrationEditorsUuids editor =
    case editor of
        KMEditor kmEditorData ->
            kmEditorData.integrations.list

        _ ->
            []


getTags : Model -> List String -> List Tag
getTags model uuids =
    List.map (getTagByTagEditorUuid model) uuids
        |> listFilterJust


getIntegrations : Model -> List String -> List Integration
getIntegrations model uuids =
    List.map (getIntegrationByIntegrationEditorUuid model) uuids
        |> listFilterJust


getTagByTagEditorUuid : Model -> String -> Maybe Tag
getTagByTagEditorUuid model uuid =
    Dict.get uuid model.editors
        |> Maybe.map
            (\t ->
                case t of
                    TagEditor tagEditorData ->
                        Just tagEditorData.tag

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Nothing


getIntegrationByIntegrationEditorUuid : Model -> String -> Maybe Integration
getIntegrationByIntegrationEditorUuid model uuid =
    Dict.get uuid model.editors
        |> Maybe.map
            (\t ->
                case t of
                    IntegrationEditor integrationEditorData ->
                        Just integrationEditorData.integration

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Nothing


insertEditor : Editor -> Model -> Model
insertEditor editor model =
    let
        newEditors =
            Dict.insert (getEditorUuid editor) editor model.editors
    in
    { model | editors = newEditors }


setAlert : String -> Model -> Model
setAlert alert model =
    { model | alert = Just alert }


getEditorContext : Model -> EditorContext
getEditorContext model =
    { metrics = model.metrics
    , levels = model.levels
    }


containsChanges : Model -> Bool
containsChanges model =
    let
        activeEditorDirty =
            getActiveEditor model
                |> Maybe.map isEditorDirty
                |> Maybe.withDefault False

        hasEvents =
            List.length model.events > 0
    in
    activeEditorDirty || hasEvents
