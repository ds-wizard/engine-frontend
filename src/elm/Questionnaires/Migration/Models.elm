module Questionnaires.Migration.Models exposing
    ( Model
    , areQuestionDetailsChanged
    , initialModel
    , initializeChangeList
    , isQuestionChangeResolved
    , isSelectedChangeResolved
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models
import FormEngine.Model exposing (FormValue, getAnswerUuid)
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Questionnaires.Common.AnswerChange exposing (AnswerAddData, AnswerChange(..), AnswerChangeData)
import Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionAddData, QuestionChange(..), QuestionChangeData)
import Questionnaires.Common.QuestionnaireChanges as QuestionnaireChanges exposing (QuestionnaireChanges)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Utils exposing (flip, listFilterJust)


type alias Model =
    { questionnaireUuid : String
    , questionnaireMigration : ActionResult QuestionnaireMigration
    , levels : ActionResult (List Level)
    , changes : QuestionnaireChanges
    , selectedChange : Maybe QuestionChange
    , questionnaireModel : Maybe Common.Questionnaire.Models.Model
    }


initialModel : String -> Model
initialModel questionnaireUuid =
    { questionnaireUuid = questionnaireUuid
    , questionnaireMigration = Loading
    , levels = Loading
    , changes = QuestionnaireChanges.empty
    , selectedChange = Nothing
    , questionnaireModel = Nothing
    }


isSelectedChangeResolved : Model -> Bool
isSelectedChangeResolved model =
    let
        isResolved questionUuid =
            model.questionnaireMigration
                |> ActionResult.map (QuestionnaireMigration.isQuestionResolved questionUuid)
                |> ActionResult.withDefault False
    in
    model.selectedChange
        |> Maybe.map (QuestionChange.getQuestionUuid >> isResolved)
        |> Maybe.withDefault False


initializeChangeList : AppState -> Model -> Model
initializeChangeList appState model =
    case model.questionnaireMigration of
        Success migration ->
            let
                changes =
                    getChangeList appState migration

                selectedChange =
                    changes.questions
                        |> List.filter (QuestionChange.getQuestionUuid >> flip QuestionnaireMigration.isQuestionResolved migration >> not)
                        |> List.head
                        |> Maybe.orElse (List.head changes.questions)
            in
            { model | changes = changes, selectedChange = selectedChange }

        _ ->
            { model | changes = QuestionnaireChanges.empty }


getChangeList : AppState -> QuestionnaireMigration -> QuestionnaireChanges
getChangeList appState migration =
    KnowledgeModel.getChapters migration.newQuestionnaire.knowledgeModel
        |> QuestionnaireChanges.foldMap (getChapterChanges appState migration)


getChapterChanges : AppState -> QuestionnaireMigration -> Chapter -> QuestionnaireChanges
getChapterChanges appState migration chapter =
    QuestionnaireChanges.foldMap
        (getQuestionChanges appState migration chapter)
        (KnowledgeModel.getChapterQuestions chapter.uuid migration.newQuestionnaire.knowledgeModel)


getQuestionChanges : AppState -> QuestionnaireMigration -> Chapter -> Question -> QuestionnaireChanges
getQuestionChanges appState migration chapter question =
    let
        oldKm =
            migration.oldQuestionnaire.knowledgeModel

        newKm =
            migration.newQuestionnaire.knowledgeModel

        questionChange =
            --            case List.find (Question.getUuid >> (==) (Question.getUuid question)) (KnowledgeModel.getQuestions migration.oldQuestionnaire.knowledgeModel) of
            case KnowledgeModel.getQuestion (Question.getUuid question) oldKm of
                Just oldQuestion ->
                    let
                        answerChanges =
                            getAnswerChanges migration question
                    in
                    if not (List.isEmpty answerChanges) || isChanged appState.config.levelsEnabled oldQuestion question then
                        QuestionnaireChanges [ QuestionChange <| QuestionChangeData question oldQuestion chapter ] answerChanges

                    else
                        QuestionnaireChanges.empty

                Nothing ->
                    if isNew migration.oldQuestionnaire question then
                        QuestionnaireChanges [ QuestionAdd <| QuestionAddData question chapter ] []

                    else
                        QuestionnaireChanges.empty

        childChanges =
            case getReply migration.newQuestionnaire question of
                Just formValue ->
                    case question of
                        OptionsQuestion _ _ ->
                            case KnowledgeModel.getAnswer (getAnswerUuid formValue.value) newKm of
                                Just answer ->
                                    QuestionnaireChanges.foldMap
                                        (getQuestionChanges appState migration chapter)
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid migration.newQuestionnaire.knowledgeModel)

                                Nothing ->
                                    QuestionnaireChanges.empty

                        ListQuestion commonData _ ->
                            QuestionnaireChanges.foldMap
                                (getQuestionChanges appState migration chapter)
                                (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid migration.newQuestionnaire.knowledgeModel)

                        _ ->
                            QuestionnaireChanges.empty

                _ ->
                    QuestionnaireChanges.empty
    in
    QuestionnaireChanges.merge questionChange childChanges


getReply : QuestionnaireDetail -> Question -> Maybe FormValue
getReply questionnaire question =
    List.find (.path >> getUuidFromPath >> (==) (Question.getUuid question)) questionnaire.replies


isNew : QuestionnaireDetail -> Question -> Bool
isNew questionnaire question =
    Maybe.isNothing <| KnowledgeModel.getQuestion (Question.getUuid question) questionnaire.knowledgeModel


isChanged : Bool -> Question -> Question -> Bool
isChanged levelsEnabled oldQuestion newQuestion =
    (Question.getTitle oldQuestion /= Question.getTitle newQuestion)
        || areQuestionDetailsChanged levelsEnabled oldQuestion newQuestion


areQuestionDetailsChanged : Bool -> Question -> Question -> Bool
areQuestionDetailsChanged levelsEnabled oldQuestion newQuestion =
    (Question.getText oldQuestion /= Question.getText newQuestion)
        || (levelsEnabled && (Question.getRequiredLevel oldQuestion /= Question.getRequiredLevel newQuestion))
        || (Question.getReferenceUuids oldQuestion /= Question.getReferenceUuids newQuestion)
        || (Question.getExpertUuids oldQuestion /= Question.getExpertUuids newQuestion)


getAnswerChanges : QuestionnaireMigration -> Question -> List AnswerChange
getAnswerChanges migration newQuestion =
    let
        oldKm =
            migration.oldQuestionnaire.knowledgeModel

        newKm =
            migration.newQuestionnaire.knowledgeModel

        createAnswerChange answer =
            case KnowledgeModel.getAnswer answer.uuid oldKm of
                Just oldAnswer ->
                    if oldAnswer.label /= answer.label then
                        Just <| AnswerChange <| AnswerChangeData answer oldAnswer

                    else
                        Nothing

                Nothing ->
                    Just <| AnswerAdd <| AnswerAddData answer
    in
    KnowledgeModel.getQuestionAnswers (Question.getUuid newQuestion) newKm
        |> List.map createAnswerChange
        |> listFilterJust


getUuidFromPath : String -> String
getUuidFromPath path =
    String.split "." path
        |> List.last
        |> Maybe.withDefault ""


isQuestionChangeResolved : QuestionnaireMigration -> QuestionChange -> Bool
isQuestionChangeResolved migration change =
    List.member (QuestionChange.getQuestionUuid change) migration.resolvedQuestionUuids
