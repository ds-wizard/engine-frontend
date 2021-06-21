module Wizard.KMEditor.Editor.KMEditor.Models exposing
    ( Model
    , containsChanges
    , getActiveEditor
    , getCurrentIntegrations
    , getCurrentMetrics
    , getCurrentPhases
    , getCurrentTags
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
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Utils exposing (listFilterJust)
import SplitPane exposing (Orientation(..), configureSplitter, percentage)
import Uuid exposing (Uuid)
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), EditorState(..), KMEditorData, createKnowledgeModelEditor, getEditorUuid, getNewState, isEditorDirty)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (initKnowledgeModelFrom)


type alias Model =
    { kmUuid : String
    , knowledgeModel : KnowledgeModel
    , activeEditorUuid : Maybe String
    , editors : Dict String Editor
    , reorderableState : Reorderable.State
    , events : List Event
    , alert : Maybe String
    , splitPane : SplitPane.State
    , moveModal : MoveModal.Model
    }


initialModel : KnowledgeModel -> Maybe String -> List Event -> Model
initialModel knowledgeModel mbActiveEditorUuid =
    let
        activeEditorUuid =
            Maybe.or mbActiveEditorUuid (Just (Uuid.toString knowledgeModel.uuid))
    in
    createEditors mbActiveEditorUuid
        { kmUuid = Uuid.toString knowledgeModel.uuid
        , knowledgeModel = knowledgeModel
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

        AddMetricEvent _ _ ->
            AddedEdited

        EditMetricEvent _ _ ->
            Edited

        DeleteMetricEvent _ ->
            Deleted

        AddPhaseEvent _ _ ->
            AddedEdited

        EditPhaseEvent _ _ ->
            Edited

        DeletePhaseEvent _ ->
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

        AddChoiceEvent _ _ ->
            AddedEdited

        EditChoiceEvent _ _ ->
            Edited

        DeleteChoiceEvent _ ->
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

        MoveChoiceEvent _ _ ->
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


getCurrentMetrics : Model -> List Metric
getCurrentMetrics model =
    getKMEditor model
        |> Maybe.map (getMetricEditorsUuids >> getMetrics model)
        |> Maybe.withDefault []


getCurrentPhases : Model -> List Phase
getCurrentPhases model =
    getKMEditor model
        |> Maybe.map (getPhaseEditorsUuids >> getPhases model)
        |> Maybe.withDefault []


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


getMetricEditorsUuids : Editor -> List String
getMetricEditorsUuids editor =
    case editor of
        KMEditor kmEditorData ->
            kmEditorData.metrics.list

        _ ->
            []


getPhaseEditorsUuids : Editor -> List String
getPhaseEditorsUuids editor =
    case editor of
        KMEditor kmEditorData ->
            kmEditorData.phases.list

        _ ->
            []


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


getMetrics : Model -> List String -> List Metric
getMetrics model uuids =
    List.map (getMetricByMetricEditorUuid model) uuids
        |> listFilterJust


getPhases : Model -> List String -> List Phase
getPhases model uuids =
    List.map (getPhaseByPhaseEditorUuid model) uuids
        |> listFilterJust


getTags : Model -> List String -> List Tag
getTags model uuids =
    List.map (getTagByTagEditorUuid model) uuids
        |> listFilterJust


getIntegrations : Model -> List String -> List Integration
getIntegrations model uuids =
    List.map (getIntegrationByIntegrationEditorUuid model) uuids
        |> listFilterJust


getMetricByMetricEditorUuid : Model -> String -> Maybe Metric
getMetricByMetricEditorUuid model uuid =
    Dict.get uuid model.editors
        |> Maybe.map
            (\t ->
                case t of
                    MetricEditor metricEditorData ->
                        Just metricEditorData.metric

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Nothing


getPhaseByPhaseEditorUuid : Model -> String -> Maybe Phase
getPhaseByPhaseEditorUuid model uuid =
    Dict.get uuid model.editors
        |> Maybe.map
            (\t ->
                case t of
                    PhaseEditor phaseEditorData ->
                        Just phaseEditorData.phase

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Nothing


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
