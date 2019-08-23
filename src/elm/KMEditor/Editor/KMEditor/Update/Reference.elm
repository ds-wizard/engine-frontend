module KMEditor.Editor.KMEditor.Update.Reference exposing
    ( deleteReference
    , removeReference
    , updateIfQuestion
    , updateReferenceForm
    , withGenerateReferenceEditEvent
    )

import Common.AppState exposing (AppState)
import Common.Locale exposing (l)
import Form
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), QuestionEditorData, ReferenceEditorData, isReferenceEditorDirty, updateReferenceEditorData)
import KMEditor.Editor.KMEditor.Models.Forms exposing (referenceFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddReferenceEvent, createDeleteReferenceEvent, createEditReferenceEvent)
import Msgs
import Random exposing (Seed)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.KMEditor.Update.Reference"


updateReferenceForm : Model -> Form.Msg -> ReferenceEditorData -> Model
updateReferenceForm =
    updateForm
        { formValidation = referenceFormValidation
        , createEditor = ReferenceEditor
        }


withGenerateReferenceEditEvent : AppState -> Seed -> Model -> ReferenceEditorData -> (Seed -> Model -> ReferenceEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
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
