module KMEditor.Editor2.KMEditor.Update.Question exposing
    ( addAnswer
    , addAnswerItemTemplateQuestion
    , addExpert
    , addQuestionTag
    , addReference
    , deleteQuestion
    , removeQuestion
    , removeQuestionTag
    , updateIfChapterEditor
    , updateQuestionForm
    , withGenerateQuestionEditEvent
    )

import Form
import KMEditor.Common.Models.Entities exposing (newAnswer, newExpert, newQuestion, newReference)
import KMEditor.Common.Models.Path exposing (PathNode(..))
import KMEditor.Editor2.KMEditor.Models exposing (Model, getCurrentTags, insertEditor)
import KMEditor.Editor2.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor2.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor2.KMEditor.Models.Forms exposing (questionFormValidation)
import KMEditor.Editor2.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor2.KMEditor.Update.Events exposing (createAddQuestionEvent, createDeleteQuestionEvent, createEditQuestionEvent)
import Msgs
import Random exposing (Seed)


updateQuestionForm : Model -> Form.Msg -> QuestionEditorData -> Model
updateQuestionForm =
    updateForm
        { formValidation = questionFormValidation
        , createEditor = QuestionEditor
        }


addQuestionTag : Model -> String -> QuestionEditorData -> Model
addQuestionTag model uuid editorData =
    let
        newEditor =
            QuestionEditor
                { editorData
                    | tagUuids = filterTagUuids model <| uuid :: editorData.tagUuids
                }
    in
    insertEditor newEditor model


removeQuestionTag : Model -> String -> QuestionEditorData -> Model
removeQuestionTag model uuid editorData =
    let
        newEditor =
            QuestionEditor
                { editorData
                    | tagUuids = filterTagUuids model <| List.filter ((/=) uuid) editorData.tagUuids
                }
    in
    insertEditor newEditor model


filterTagUuids : Model -> List String -> List String
filterTagUuids model uuids =
    let
        currentTagUuids =
            getCurrentTags model |> List.map .uuid
    in
    List.filter (\uuid -> List.member uuid currentTagUuids) uuids


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


addAnswer : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addAnswer =
    addEntity
        { newEntity = newAnswer
        , createEntityEditor = createAnswerEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionAnswer
        }


addAnswerItemTemplateQuestion : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addAnswerItemTemplateQuestion =
    addEntity
        { newEntity = newQuestion
        , createEntityEditor = createQuestionEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionAnswerItemTemplateQuestion
        }


addReference : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addReference =
    addEntity
        { newEntity = newReference
        , createEntityEditor = createReferenceEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionReference
        }


addExpert : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addExpert =
    addEntity
        { newEntity = newExpert
        , createEntityEditor = createExpertEditor
        , createPathNode = QuestionPathNode
        , addEntity = addQuestionExpert
        }
