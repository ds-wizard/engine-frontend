module Questionnaires.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Dict exposing (Dict)
import FormEngine.Model exposing (Form, FormItem(ChoiceFormItem, GroupFormItem, StringFormItem), FormItemDescriptor, FormTree, Option(DetailedOption, SimpleOption), OptionDescriptor, createForm)
import FormEngine.Update exposing (updateForm)
import Jwt
import KnowledgeModels.Editor.Models.Entities exposing (Answer, AnswerItemTemplate, AnswerItemTemplateQuestions(..), Chapter, FollowUps(..), Question)
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (getQuestionnaire)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    getQuestionnaire uuid session
        |> Jwt.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetQuestionnaireCompleted result ->
            getQuestionnaireCompleted model result

        SetActiveChapter chapter ->
            let
                newModel =
                    { model | activeChapter = Just chapter }
            in
            ( setActiveChapterForm newModel, Cmd.none )

        FormMsg msg ->
            let
                newModel =
                    case model.activeChapterForm of
                        Just form ->
                            { model | activeChapterForm = Just <| updateForm msg form }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )


getQuestionnaireCompleted : Model -> Result Jwt.JwtError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
getQuestionnaireCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model
                        | questionnaire = Success questionnaireDetail
                        , activeChapter = List.head questionnaireDetail.knowledgeModel.chapters
                    }

                Err error ->
                    { model | questionnaire = Error "Unable to get questionnaire." }
    in
    ( setActiveChapterForm newModel, Cmd.none )


setActiveChapterForm : Model -> Model
setActiveChapterForm model =
    case model.activeChapter of
        Just chapter ->
            { model | activeChapterForm = Just <| createChapterForm chapter }

        _ ->
            model


createChapterForm : Chapter -> Form
createChapterForm chapter =
    createForm { items = List.map createQuestionFormItem chapter.questions } { values = Dict.empty }


createQuestionFormItem : Question -> FormItem
createQuestionFormItem question =
    let
        descriptor =
            createFormItemDescriptor question
    in
    case question.type_ of
        "options" ->
            ChoiceFormItem descriptor (List.map createAnswerOption (question.answers |> Maybe.withDefault []))

        "list" ->
            GroupFormItem descriptor (createGroupItems question)

        _ ->
            StringFormItem descriptor


createFormItemDescriptor : Question -> FormItemDescriptor
createFormItemDescriptor question =
    { name = question.uuid
    , label = question.title
    , text = Just question.text
    }


createAnswerOption : Answer -> Option
createAnswerOption answer =
    let
        descriptor =
            createOptionFormDescriptor answer
    in
    case answer.followUps of
        FollowUps [] ->
            SimpleOption descriptor

        FollowUps followUps ->
            DetailedOption descriptor (List.map createQuestionFormItem followUps)


createOptionFormDescriptor : Answer -> OptionDescriptor
createOptionFormDescriptor answer =
    { name = answer.uuid
    , label = answer.label
    , text = answer.advice
    }


createGroupItems : Question -> List FormItem
createGroupItems question =
    case question.answerItemTemplate of
        Just answerItemTemplate ->
            let
                itemName =
                    StringFormItem { name = "itemName", label = answerItemTemplate.title, text = Nothing }

                questions =
                    List.map createQuestionFormItem <| getQuestions answerItemTemplate.questions
            in
            itemName :: questions

        _ ->
            []


getQuestions : AnswerItemTemplateQuestions -> List Question
getQuestions (AnswerItemTemplateQuestions questions) =
    questions
