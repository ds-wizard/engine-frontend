module Wizard.KMEditor.Editor.KMEditor.Update.Metric exposing (..)

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), KMEditorData, MetricEditorData, QuestionEditorData, TagEditorData, isMetricEditorDirty, isTagEditorDirty, updateMetricEditorData, updateTagEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (metricFormValidation, tagFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddMetricEvent, createAddTagEvent, createDeleteMetricEvent, createDeleteTagEvent, createEditMetricEvent, createEditTagEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Metric"


updateMetricForm : Model -> Form.Msg -> MetricEditorData -> Model
updateMetricForm =
    updateForm
        { formValidation = metricFormValidation
        , createEditor = MetricEditor
        }


withGenerateMetricEditEvent :
    AppState
    -> Seed
    -> Model
    -> MetricEditorData
    -> (Seed -> Model -> MetricEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg ))
    -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateMetricEditEvent appState =
    withGenerateEvent
        { isDirty = isMetricEditorDirty
        , formValidation = metricFormValidation
        , createEditor = MetricEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddMetricEvent
        , createEditEvent = createEditMetricEvent
        , updateEditorData = updateMetricEditorData
        , updateEditors = Nothing
        }


deleteMetric : Seed -> Model -> String -> MetricEditorData -> ( Seed, Model )
deleteMetric =
    deleteEntity
        { removeEntity = removeMetric
        , createEditor = MetricEditor
        , createDeleteEvent = createDeleteMetricEvent
        }


removeMetric : (String -> Children -> Children) -> String -> Editor -> Editor
removeMetric removeFn uuid =
    updateIfKMEditor (\data -> { data | metrics = removeFn uuid data.metrics })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor
