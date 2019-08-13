module KMEditor.Editor.KMEditor.Update.Question exposing
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
import KMEditor.Common.KnowledgeModel.Answer as Answer
import KMEditor.Common.KnowledgeModel.Expert as Expert
import KMEditor.Common.KnowledgeModel.Question as Question
import KMEditor.Common.KnowledgeModel.Reference as Reference
import KMEditor.Editor.KMEditor.Models exposing (Model, getCurrentIntegrations, getCurrentTags, insertEditor)
import KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (questionFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddQuestionEvent, createDeleteQuestionEvent, createEditQuestionEvent)
import Msgs
import Random exposing (Seed)


updateQuestionForm : Model -> Form.Msg -> QuestionEditorData -> Model
updateQuestionForm model =
    updateForm
        { formValidation = questionFormValidation (getCurrentIntegrations model)
        , createEditor = QuestionEditor
        }
        model


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
withGenerateQuestionEditEvent seed model =
    withGenerateEvent
        { isDirty = isQuestionEditorDirty
        , formValidation = questionFormValidation (getCurrentIntegrations model)
        , createEditor = QuestionEditor
        , alert = "Please fix the question errors first."
        , createAddEvent = createAddQuestionEvent
        , createEditEvent = createEditQuestionEvent
        , updateEditorData = updateQuestionEditorData
        , updateEditors = Just updateEditorsWithQuestion
        }
        seed
        model


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
        { newEntity = Answer.new
        , createEntityEditor = createAnswerEditor
        , addEntity = addQuestionAnswer
        }


addAnswerItemTemplateQuestion : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addAnswerItemTemplateQuestion =
    addEntity
        { newEntity = Question.new
        , createEntityEditor = createQuestionEditor
        , addEntity = addQuestionAnswerItemTemplateQuestion
        }


addReference : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addReference =
    addEntity
        { newEntity = Reference.new
        , createEntityEditor = createReferenceEditor
        , addEntity = addQuestionReference
        }


addExpert : Cmd Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addExpert =
    addEntity
        { newEntity = Expert.new
        , createEntityEditor = createExpertEditor
        , addEntity = addQuestionExpert
        }
