module KMEditor.Editor.KMEditor.Update.Answer exposing
    ( addFollowUp
    , deleteAnswer
    , removeAnswer
    , updateAnswerForm
    , updateIfQuestion
    , withGenerateAnswerEditEvent
    )

import Form
import KMEditor.Common.KnowledgeModel.Question as Question
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (AnswerEditorData, Editor(..), QuestionEditorData, addAnswerFollowUp, createQuestionEditor, isAnswerEditorDirty, updateAnswerEditorData)
import KMEditor.Editor.KMEditor.Models.Forms exposing (answerFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddAnswerEvent, createDeleteAnswerEvent, createEditAnswerEvent)
import Msgs
import Random exposing (Seed)


updateAnswerForm : Model -> Form.Msg -> AnswerEditorData -> Model
updateAnswerForm =
    updateForm
        { formValidation = answerFormValidation
        , createEditor = AnswerEditor
        }


withGenerateAnswerEditEvent : Seed -> Model -> AnswerEditorData -> (Seed -> Model -> AnswerEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateAnswerEditEvent =
    withGenerateEvent
        { isDirty = isAnswerEditorDirty
        , formValidation = answerFormValidation
        , createEditor = AnswerEditor
        , alert = "Please fix the answer errors first."
        , createAddEvent = createAddAnswerEvent
        , createEditEvent = createEditAnswerEvent
        , updateEditorData = updateAnswerEditorData
        , updateEditors = Nothing
        }


deleteAnswer : Seed -> Model -> String -> AnswerEditorData -> ( Seed, Model )
deleteAnswer =
    deleteEntity
        { removeEntity = removeAnswer
        , createEditor = AnswerEditor
        , createDeleteEvent = createDeleteAnswerEvent
        }


removeAnswer : (String -> Children -> Children) -> String -> Editor -> Editor
removeAnswer removeFn uuid =
    updateIfQuestion (\data -> { data | answers = removeFn uuid data.answers })


updateIfQuestion : (QuestionEditorData -> QuestionEditorData) -> Editor -> Editor
updateIfQuestion update editor =
    case editor of
        QuestionEditor kmEditorData ->
            QuestionEditor <| update kmEditorData

        _ ->
            editor


addFollowUp : Cmd Msgs.Msg -> Seed -> Model -> AnswerEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addFollowUp =
    addEntity
        { newEntity = Question.new
        , createEntityEditor = createQuestionEditor
        , addEntity = addAnswerFollowUp
        }
