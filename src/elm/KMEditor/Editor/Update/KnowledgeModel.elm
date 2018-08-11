module KMEditor.Editor.Update.KnowledgeModel exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (newChapter)
import KMEditor.Common.Models.Path exposing (PathNode(KMPathNode))
import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (knowledgeModelFormValidation)
import KMEditor.Editor.Update.Abstract exposing (addEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.Update.Events exposing (createEditKnowledgeModelEvent)
import Msgs
import Random.Pcg exposing (Seed)


updateKMForm : Model -> Form.Msg -> KMEditorData -> Model
updateKMForm =
    updateForm
        { formValidation = knowledgeModelFormValidation
        , createEditor = KMEditor
        }


withGenerateKMEditEvent : Seed -> Model -> KMEditorData -> (Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateKMEditEvent =
    withGenerateEvent
        { isDirty = isKMEditorDirty
        , formValidation = knowledgeModelFormValidation
        , createEditor = KMEditor
        , alert = "Please fix the knowledge model errors first."
        , createAddEvent = createEditKnowledgeModelEvent
        , createEditEvent = createEditKnowledgeModelEvent
        , updateEditorData = updateKMEditorData
        , updateEditors = Nothing
        }


addChapter : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addChapter =
    addEntity
        { newEntity = newChapter
        , createEntityEditor = createChapterEditor
        , createPathNode = KMPathNode
        , addEntity = addKMChapter
        }
