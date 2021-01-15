module Wizard.KMEditor.Editor.KMEditor.Update.Choice exposing (deleteChoice, removeChoice, updateChoiceForm, updateIfQuestion, withGenerateChoiceEditEvent)

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (ChoiceEditorData, Editor(..), ExpertEditorData, QuestionEditorData, isChoiceEditorDirty, updateChoiceEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (choiceFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddChoiceEvent, createDeleteChoiceEvent, createEditChoiceEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Choice"


updateChoiceForm : Model -> Form.Msg -> ChoiceEditorData -> Model
updateChoiceForm =
    updateForm
        { formValidation = choiceFormValidation
        , createEditor = ChoiceEditor
        }


withGenerateChoiceEditEvent : AppState -> Seed -> Model -> ChoiceEditorData -> (Seed -> Model -> ChoiceEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateChoiceEditEvent appState =
    withGenerateEvent
        { isDirty = isChoiceEditorDirty
        , formValidation = choiceFormValidation
        , createEditor = ChoiceEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddChoiceEvent
        , createEditEvent = createEditChoiceEvent
        , updateEditorData = updateChoiceEditorData
        , updateEditors = Nothing
        }


deleteChoice : Seed -> Model -> String -> ChoiceEditorData -> ( Seed, Model )
deleteChoice =
    deleteEntity
        { removeEntity = removeChoice
        , createEditor = ChoiceEditor
        , createDeleteEvent = createDeleteChoiceEvent
        }


removeChoice : (String -> Children -> Children) -> String -> Editor -> Editor
removeChoice removeFn uuid =
    updateIfQuestion (\data -> { data | choices = removeFn uuid data.choices })


updateIfQuestion : (QuestionEditorData -> QuestionEditorData) -> Editor -> Editor
updateIfQuestion update editor =
    case editor of
        QuestionEditor kmEditorData ->
            QuestionEditor <| update kmEditorData

        _ ->
            editor
