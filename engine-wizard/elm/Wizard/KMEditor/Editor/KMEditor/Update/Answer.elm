module Wizard.KMEditor.Editor.KMEditor.Update.Answer exposing
    ( addFollowUp
    , deleteAnswer
    , removeAnswer
    , updateAnswerForm
    , updateIfQuestion
    , withGenerateAnswerEditEvent
    )

import Form
import Random exposing (Seed)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model, getCurrentIntegrations, getCurrentMetrics)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (AnswerEditorData, Editor(..), QuestionEditorData, addAnswerFollowUp, createQuestionEditor, isAnswerEditorDirty, updateAnswerEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (answerFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddAnswerEvent, createDeleteAnswerEvent, createEditAnswerEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Answer"


updateAnswerForm : Model -> Form.Msg -> AnswerEditorData -> Model
updateAnswerForm model =
    updateForm
        { formValidation = answerFormValidation (getCurrentMetrics model)
        , createEditor = AnswerEditor
        }
        model


withGenerateAnswerEditEvent : AppState -> Seed -> Model -> AnswerEditorData -> (Seed -> Model -> AnswerEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateAnswerEditEvent appState seed model =
    withGenerateEvent
        { isDirty = isAnswerEditorDirty
        , formValidation = answerFormValidation (getCurrentMetrics model)
        , createEditor = AnswerEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddAnswerEvent
        , createEditEvent = createEditAnswerEvent
        , updateEditorData = updateAnswerEditorData (getCurrentMetrics model)
        , updateEditors = Nothing
        }
        seed
        model


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


addFollowUp : Cmd Wizard.Msgs.Msg -> Seed -> Model -> AnswerEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
addFollowUp cmd seed model =
    addEntity
        { newEntity = Question.new
        , createEntityEditor = createQuestionEditor (getCurrentIntegrations model)
        , addEntity = addAnswerFollowUp
        }
        cmd
        seed
        model
