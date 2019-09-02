module KMEditor.Editor.KMEditor.Update.KnowledgeModel exposing
    ( addChapter
    , addIntegration
    , addTag
    , updateKMForm
    , withGenerateKMEditEvent
    )

import Common.AppState exposing (AppState)
import Common.Locale exposing (l)
import Form
import KMEditor.Common.KnowledgeModel.Chapter as Chapter
import KMEditor.Common.KnowledgeModel.Integration as Integration
import KMEditor.Common.KnowledgeModel.Tag as Tag
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (knowledgeModelFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createEditKnowledgeModelEvent)
import Msgs
import Random exposing (Seed)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.KMEditor.Update.KnowledgeModel"


updateKMForm : Model -> Form.Msg -> KMEditorData -> Model
updateKMForm =
    updateForm
        { formValidation = knowledgeModelFormValidation
        , createEditor = KMEditor
        }


withGenerateKMEditEvent : AppState -> Seed -> Model -> KMEditorData -> (Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateKMEditEvent appState =
    withGenerateEvent
        { isDirty = isKMEditorDirty
        , formValidation = knowledgeModelFormValidation
        , createEditor = KMEditor
        , alert = l_ "alert" appState
        , createAddEvent = createEditKnowledgeModelEvent
        , createEditEvent = createEditKnowledgeModelEvent
        , updateEditorData = updateKMEditorData
        , updateEditors = Nothing
        }


addChapter : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addChapter =
    addEntity
        { newEntity = Chapter.new
        , createEntityEditor = createChapterEditor
        , addEntity = addKMChapter
        }


addTag : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addTag =
    addEntity
        { newEntity = Tag.new
        , createEntityEditor = createTagEditor
        , addEntity = addKMTag
        }


addIntegration : Cmd Msgs.Msg -> Seed -> Model -> KMEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addIntegration =
    addEntity
        { newEntity = Integration.new
        , createEntityEditor = createIntegrationEditor
        , addEntity = addKMIntegration
        }
