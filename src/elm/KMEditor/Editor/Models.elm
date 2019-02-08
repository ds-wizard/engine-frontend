module KMEditor.Editor.Models exposing
    ( Model
    , addEvent
    , containsChanges
    , getActiveEditor
    , getCurrentTags
    , getEditorContext
    , getKMEditor
    , initialModel
    , insertEditor
    , setAlert
    )

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric, Tag)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor.Models.EditorContext exposing (EditorContext)
import KMEditor.Editor.Models.Editors exposing (Editor(..), KMEditorData, getEditorTitle, getEditorUuid, isEditorDirty)
import Reorderable
import SplitPane exposing (Orientation(..), configureSplitter, percentage)
import Utils exposing (listFilterJust)


type alias Model =
    { branchUuid : String
    , kmUuid : ActionResult String
    , knowledgeModel : ActionResult KnowledgeModel
    , metrics : ActionResult (List Metric)
    , levels : ActionResult (List Level)
    , activeEditorUuid : Maybe String
    , editors : Dict String Editor
    , reorderableState : Reorderable.State
    , events : List Event
    , alert : Maybe String
    , submitting : ActionResult String
    , splitPane : SplitPane.State
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , kmUuid = Loading
    , knowledgeModel = Loading
    , metrics = Loading
    , levels = Loading
    , activeEditorUuid = Nothing
    , editors = Dict.fromList []
    , reorderableState = Reorderable.initialState
    , events = []
    , alert = Nothing
    , submitting = Unset
    , splitPane = SplitPane.init Horizontal |> configureSplitter (percentage 0.2 (Just ( 0.05, 0.7 )))
    }



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
    model.kmUuid
        |> ActionResult.map (\uuid -> Dict.get uuid model.editors)
        |> ActionResult.withDefault Nothing


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
        |> List.sortBy .name


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


addEvent : Event -> Model -> Model
addEvent event model =
    { model | events = model.events ++ [ event ] }


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
    { metrics = model.metrics |> ActionResult.withDefault []
    , levels = model.levels |> ActionResult.withDefault []
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
