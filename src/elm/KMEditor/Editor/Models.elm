module KMEditor.Editor.Models exposing (..)

import Common.Types exposing (ActionResult(Loading, Unset), withDefault)
import Dict exposing (Dict)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Metric)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor.Models.EditorContext exposing (EditorContext)
import KMEditor.Editor.Models.Editors exposing (Editor, KMEditorData, getEditorTitle, getEditorUuid)
import Reorderable
import SplitPane exposing (Orientation(Horizontal), configureSplitter, percentage)


type alias Model =
    { branchUuid : String
    , kmUuid : ActionResult String
    , knowledgeModel : ActionResult KnowledgeModel
    , metrics : ActionResult (List Metric)
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
    { metrics = model.metrics |> withDefault []
    }
