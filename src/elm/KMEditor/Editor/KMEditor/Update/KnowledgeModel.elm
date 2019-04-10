module KMEditor.Editor.KMEditor.Update.KnowledgeModel exposing
    ( addChapter
    , addIntegration
    , addTag
    , updateKMForm
    , withGenerateKMEditEvent
    )

import Form
import KMEditor.Common.Models.Entities exposing (newChapter, newIntegration, newTag)
import KMEditor.Common.Models.Path exposing (PathNode(..))
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (knowledgeModelFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createEditKnowledgeModelEvent)
import Msgs
import Random exposing (Seed)


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


addTag : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addTag =
    addEntity
        { newEntity = newTag
        , createEntityEditor = createTagEditor
        , createPathNode = KMPathNode
        , addEntity = addKMTag
        }


addIntegration : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addIntegration =
    addEntity
        { newEntity = newIntegration
        , createEntityEditor = createIntegrationEditor
        , createPathNode = KMPathNode
        , addEntity = addKMIntegration
        }
