module KMEditor.Editor.KMEditor.Models exposing
    ( Model
    , containsChanges
    , getActiveEditor
    , getCurrentTags
    , getEditorContext
    , getKMEditor
    , initialModel
    , insertEditor
    , setAlert
    )

import Dict exposing (Dict)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric, Tag)
import KMEditor.Common.Models.Events exposing (Event(..), getEventEntityUuid)
import KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)
import KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), EditorState(..), KMEditorData, createKnowledgeModelEditor, getEditorUuid, getNewState, isEditorDirty)
import Reorderable
import SplitPane exposing (Orientation(..), configureSplitter, percentage)
import Utils exposing (listFilterJust)


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
    }


initialModel : KnowledgeModel -> List Metric -> List Level -> List Event -> Model
initialModel knowledgeModel metrics levels =
    createEditors
        { kmUuid = knowledgeModel.uuid
        , knowledgeModel = knowledgeModel
        , metrics = metrics
        , levels = levels
        , activeEditorUuid = Just knowledgeModel.uuid
        , editors = Dict.fromList []
        , reorderableState = Reorderable.initialState
        , events = []
        , alert = Nothing
        , splitPane = SplitPane.init Horizontal |> configureSplitter (percentage 0.2 (Just ( 0.05, 0.7 )))
        }


createEditors : Model -> List Event -> Model
createEditors model events =
    { model | editors = createKnowledgeModelEditor (getEditorContext model) (getEditorState (createEditorStateDict events)) model.knowledgeModel model.editors }


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
                    getEventEntityUuid event

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
        EditKnowledgeModelEvent _ _ ->
            Edited

        AddChapterEvent _ _ ->
            Added

        EditChapterEvent _ _ ->
            Edited

        DeleteChapterEvent _ _ ->
            Deleted

        AddTagEvent _ _ ->
            Added

        EditTagEvent _ _ ->
            Edited

        DeleteTagEvent _ _ ->
            Deleted

        AddQuestionEvent _ _ ->
            Added

        EditQuestionEvent _ _ ->
            Edited

        DeleteQuestionEvent _ _ ->
            Deleted

        AddAnswerEvent _ _ ->
            Added

        EditAnswerEvent _ _ ->
            Edited

        DeleteAnswerEvent _ _ ->
            Deleted

        AddReferenceEvent _ _ ->
            Added

        EditReferenceEvent _ _ ->
            Edited

        DeleteReferenceEvent _ _ ->
            Deleted

        AddExpertEvent _ _ ->
            Added

        EditExpertEvent _ _ ->
            Edited

        DeleteExpertEvent _ _ ->
            Deleted



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
        |> Maybe.map (getTagEditorsUuids model >> getTags model)
        |> Maybe.withDefault []


getTagEditorsUuids : Model -> Editor -> List String
getTagEditorsUuids model editor =
    case editor of
        KMEditor kmEditorData ->
            kmEditorData.tags.list

        _ ->
            []


getTags : Model -> List String -> List Tag
getTags model uuids =
    List.map (getTagByTagEditorUuid model) uuids
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
