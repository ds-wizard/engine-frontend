module KMEditor.Editor.Update.Reference exposing (..)

import Form
import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Editors exposing (Editor(QuestionEditor, ReferenceEditor), QuestionEditorData, ReferenceEditorData, isReferenceEditorDirty, updateReferenceEditorData)
import KMEditor.Editor.Models.Forms exposing (referenceFormValidation)
import KMEditor.Editor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.Update.Events exposing (createAddReferenceEvent, createDeleteReferenceEvent, createEditReferenceEvent)
import Msgs
import Random.Pcg exposing (Seed)


updateReferenceForm : Model -> Form.Msg -> ReferenceEditorData -> Model
updateReferenceForm =
    updateForm
        { formValidation = referenceFormValidation
        , createEditor = ReferenceEditor
        }


withGenerateReferenceEditEvent : Seed -> Model -> ReferenceEditorData -> (Seed -> Model -> ReferenceEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateReferenceEditEvent =
    withGenerateEvent
        { isDirty = isReferenceEditorDirty
        , formValidation = referenceFormValidation
        , createEditor = ReferenceEditor
        , alert = "Please fix the reference errors first."
        , createAddEvent = createAddReferenceEvent
        , createEditEvent = createEditReferenceEvent
        , updateEditorData = updateReferenceEditorData
        }


deleteReference : Seed -> Model -> String -> ReferenceEditorData -> ( Seed, Model )
deleteReference =
    deleteEntity
        { removeEntity = removeReference
        , createEditor = ReferenceEditor
        , createDeleteEvent = createDeleteReferenceEvent
        }


removeReference : (String -> Children -> Children) -> String -> Editor -> Editor
removeReference removeFn uuid =
    updateIfQuestion (\data -> { data | references = removeFn uuid data.references })


updateIfQuestion : (QuestionEditorData -> QuestionEditorData) -> Editor -> Editor
updateIfQuestion update editor =
    case editor of
        QuestionEditor kmEditorData ->
            QuestionEditor <| update kmEditorData

        _ ->
            editor
