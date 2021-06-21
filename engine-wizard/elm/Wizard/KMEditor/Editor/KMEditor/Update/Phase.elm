module Wizard.KMEditor.Editor.KMEditor.Update.Phase exposing (..)

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), KMEditorData, MetricEditorData, PhaseEditorData, QuestionEditorData, TagEditorData, isPhaseEditorDirty, updatePhaseEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (phaseFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddPhaseEvent, createDeletePhaseEvent, createEditPhaseEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Phase"


updatePhaseForm : Model -> Form.Msg -> PhaseEditorData -> Model
updatePhaseForm =
    updateForm
        { formValidation = phaseFormValidation
        , createEditor = PhaseEditor
        }


withGeneratePhaseEditEvent :
    AppState
    -> Seed
    -> Model
    -> PhaseEditorData
    -> (Seed -> Model -> PhaseEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg ))
    -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGeneratePhaseEditEvent appState =
    withGenerateEvent
        { isDirty = isPhaseEditorDirty
        , formValidation = phaseFormValidation
        , createEditor = PhaseEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddPhaseEvent
        , createEditEvent = createEditPhaseEvent
        , updateEditorData = updatePhaseEditorData
        , updateEditors = Nothing
        }


deletePhase : Seed -> Model -> String -> PhaseEditorData -> ( Seed, Model )
deletePhase =
    deleteEntity
        { removeEntity = removePhase
        , createEditor = PhaseEditor
        , createDeleteEvent = createDeletePhaseEvent
        }


removePhase : (String -> Children -> Children) -> String -> Editor -> Editor
removePhase removeFn uuid =
    updateIfKMEditor (\data -> { data | phases = removeFn uuid data.phases })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor
