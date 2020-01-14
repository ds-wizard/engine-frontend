module Wizard.KMEditor.Editor.KMEditor.Update.Tag exposing
    ( deleteTag
    , removeTag
    , updateTagForm
    , withGenerateTagEditEvent
    )

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), KMEditorData, QuestionEditorData, TagEditorData, isTagEditorDirty, updateTagEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (tagFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddTagEvent, createDeleteTagEvent, createEditTagEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Tag"


updateTagForm : Model -> Form.Msg -> TagEditorData -> Model
updateTagForm =
    updateForm
        { formValidation = tagFormValidation
        , createEditor = TagEditor
        }


withGenerateTagEditEvent :
    AppState
    -> Seed
    -> Model
    -> TagEditorData
    -> (Seed -> Model -> TagEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg ))
    -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateTagEditEvent appState =
    withGenerateEvent
        { isDirty = isTagEditorDirty
        , formValidation = tagFormValidation
        , createEditor = TagEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddTagEvent
        , createEditEvent = createEditTagEvent
        , updateEditorData = updateTagEditorData
        , updateEditors = Nothing
        }


deleteTag : Seed -> Model -> String -> TagEditorData -> ( Seed, Model )
deleteTag =
    deleteEntity
        { removeEntity = removeTag
        , createEditor = TagEditor
        , createDeleteEvent = createDeleteTagEvent
        }


removeTag : (String -> Children -> Children) -> String -> Editor -> Editor
removeTag removeFn uuid =
    updateIfKMEditor (\data -> { data | tags = removeFn uuid data.tags })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor
