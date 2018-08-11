module KMEditor.Editor.Update.Answer exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (newQuestion)
import KMEditor.Common.Models.Path exposing (PathNode(AnswerPathNode))
import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Editors exposing (AnswerEditorData, Editor(AnswerEditor, QuestionEditor), QuestionEditorData, addAnswerFollowUp, createQuestionEditor, isAnswerEditorDirty, updateAnswerEditorData)
import KMEditor.Editor.Models.Forms exposing (answerFormValidation)
import KMEditor.Editor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.Update.Events exposing (createAddAnswerEvent, createDeleteAnswerEvent, createEditAnswerEvent)
import Msgs
import Random.Pcg exposing (Seed)


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
        { newEntity = newQuestion
        , createEntityEditor = createQuestionEditor
        , createPathNode = AnswerPathNode
        , addEntity = addAnswerFollowUp
        }
