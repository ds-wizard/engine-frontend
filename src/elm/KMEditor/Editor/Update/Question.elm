module KMEditor.Editor.Update.Question exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (newAnswer, newExpert, newQuestion, newReference)
import KMEditor.Common.Models.Path exposing (PathNode(QuestionPathNode))
import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (questionFormValidation)
import KMEditor.Editor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.Update.Events exposing (createAddQuestionEvent, createDeleteQuestionEvent, createEditQuestionEvent)
import Msgs
import Random.Pcg exposing (Seed)


updateQuestionForm : Model -> Form.Msg -> QuestionEditorData -> Model
updateQuestionForm =
    updateForm
        { formValidation = questionFormValidation
        , createEditor = QuestionEditor
        }


withGenerateQuestionEditEvent : Seed -> Model -> QuestionEditorData -> (Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateQuestionEditEvent =
    withGenerateEvent
        { isDirty = isQuestionEditorDirty
        , formValidation = questionFormValidation
        , createEditor = QuestionEditor
        , alert = "Please fix the question errors first."
        , createAddEvent = createAddQuestionEvent
        , createEditEvent = createEditQuestionEvent
        , updateEditorData = updateQuestionEditorData
        , updateEditors = Just updateEditorsWithQuestion
        }


deleteQuestion : Seed -> Model -> String -> QuestionEditorData -> ( Seed, Model )
deleteQuestion =
    deleteEntity
        { removeEntity = removeQuestion
        , createEditor = QuestionEditor
        , createDeleteEvent = createDeleteQuestionEvent
        }


removeQuestion : (String -> Children -> Children) -> String -> Editor -> Editor
removeQuestion removeFn uuid =
    updateIfChapterEditor (\data -> { data | questions = removeFn uuid data.questions })


updateIfChapterEditor : (ChapterEditorData -> ChapterEditorData) -> Editor -> Editor
updateIfChapterEditor update editor =
    case editor of
        ChapterEditor editorData ->
            ChapterEditor <| update editorData

        _ ->
            editor


addAnswer : Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addAnswer =
    addEntity
        { newEntity = newAnswer
        , createEntityEditor = createAnswerEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionAnswer
        }


addAnswerItemTemplateQuestion : Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addAnswerItemTemplateQuestion =
    addEntity
        { newEntity = newQuestion
        , createEntityEditor = createQuestionEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionAnswerItemTemplateQuestion
        }


addReference : Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addReference =
    addEntity
        { newEntity = newReference
        , createEntityEditor = createReferenceEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionReference
        }


addExpert : Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addExpert =
    addEntity
        { newEntity = newExpert
        , createEntityEditor = createExpertEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionExpert
        }
