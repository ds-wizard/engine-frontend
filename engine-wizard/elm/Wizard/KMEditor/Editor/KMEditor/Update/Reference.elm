module Wizard.KMEditor.Editor.KMEditor.Update.Reference exposing
    ( deleteReference
    , removeReference
    , updateIfQuestion
    , updateReferenceForm
    , withGenerateReferenceEditEvent
    )

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), QuestionEditorData, ReferenceEditorData, isReferenceEditorDirty, updateReferenceEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (referenceFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddReferenceEvent, createDeleteReferenceEvent, createEditReferenceEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Reference"


updateReferenceForm : Model -> Form.Msg -> ReferenceEditorData -> Model
updateReferenceForm =
    updateForm
        { formValidation = referenceFormValidation
        , createEditor = ReferenceEditor
        }


withGenerateReferenceEditEvent : AppState -> Seed -> Model -> ReferenceEditorData -> (Seed -> Model -> ReferenceEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateReferenceEditEvent appState =
    withGenerateEvent
        { isDirty = isReferenceEditorDirty
        , formValidation = referenceFormValidation
        , createEditor = ReferenceEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddReferenceEvent
        , createEditEvent = createEditReferenceEvent
        , updateEditorData = updateReferenceEditorData
        , updateEditors = Nothing
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
