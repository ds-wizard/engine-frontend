module Wizard.KMEditor.Editor.KMEditor.Update.Question exposing
    ( addAnswer
    , addAnswerItemTemplateQuestion
    , addChoice
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
import Random exposing (Seed)
import Shared.Data.KnowledgeModel.Answer as Answer
import Shared.Data.KnowledgeModel.Choice as Choice
import Shared.Data.KnowledgeModel.Expert as Expert
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.KnowledgeModel.Reference as Reference
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model, getCurrentIntegrations, getCurrentTags, insertEditor)
import Wizard.KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (questionFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddQuestionEvent, createDeleteQuestionEvent, createEditQuestionEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Question"


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


withGenerateQuestionEditEvent : AppState -> Seed -> Model -> QuestionEditorData -> (Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateQuestionEditEvent appState seed model =
    withGenerateEvent
        { isDirty = isQuestionEditorDirty
        , formValidation = questionFormValidation (getCurrentIntegrations model)
        , createEditor = QuestionEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddQuestionEvent
        , createEditEvent = createEditQuestionEvent
        , updateEditorData = updateQuestionEditorData (getCurrentIntegrations model)
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


addAnswer : Cmd Wizard.Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addAnswer cmd seed model =
    addEntity
        { newEntity = Answer.new
        , createEntityEditor = createAnswerEditor (getCurrentIntegrations model)
        , addEntity = addQuestionAnswer
        }
        cmd
        seed
        model


addAnswerItemTemplateQuestion : Cmd Wizard.Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addAnswerItemTemplateQuestion cmd seed model =
    addEntity
        { newEntity = Question.new
        , createEntityEditor = createQuestionEditor (getCurrentIntegrations model)
        , addEntity = addQuestionAnswerItemTemplateQuestion
        }
        cmd
        seed
        model


addChoice : Cmd Wizard.Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addChoice =
    addEntity
        { newEntity = Choice.new
        , createEntityEditor = createChoiceEditor
        , addEntity = addQuestionChoice
        }


addReference : Cmd Wizard.Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addReference =
    addEntity
        { newEntity = Reference.new
        , createEntityEditor = createReferenceEditor
        , addEntity = addQuestionReference
        }


addExpert : Cmd Wizard.Msgs.Msg -> Seed -> Model -> QuestionEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addExpert =
    addEntity
        { newEntity = Expert.new
        , createEntityEditor = createExpertEditor
        , addEntity = addQuestionExpert
        }
