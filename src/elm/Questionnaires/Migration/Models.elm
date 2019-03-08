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
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import FormEngine.Model exposing (FormValue, getAnswerUuid)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, Level, Question(..), getChapters, getFollowUpQuestions, getQuestionAnswers, getQuestionExperts, getQuestionReferences, getQuestionRequiredLevel, getQuestionText, getQuestionTitle, getQuestionUuid, getQuestions)
import List.Extra as List
import Maybe.Extra as Maybe
import Questionnaires.Common.AnswerChange exposing (AnswerAddData, AnswerChange(..), AnswerChangeData)
import Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionAddData, QuestionChange(..), QuestionChangeData)
import Questionnaires.Common.QuestionnaireChanges as QuestionnaireChanges exposing (QuestionnaireChanges)
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
    getChapters migration.newQuestionnaire.knowledgeModel
        |> QuestionnaireChanges.foldMap (getChapterChanges appState migration)


getChapterChanges : AppState -> QuestionnaireMigration -> Chapter -> QuestionnaireChanges
getChapterChanges appState migration chapter =
    QuestionnaireChanges.foldMap (getQuestionChanges appState migration chapter) chapter.questions


getQuestionChanges : AppState -> QuestionnaireMigration -> Chapter -> Question -> QuestionnaireChanges
getQuestionChanges appState migration chapter question =
    let
        questionChange =
            case List.find (getQuestionUuid >> (==) (getQuestionUuid question)) (getQuestions migration.oldQuestionnaire.knowledgeModel) of
                Just oldQuestion ->
                    let
                        answerChanges =
                            getAnswerChanges oldQuestion question
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
                        OptionsQuestion questionData ->
                            case List.find (.uuid >> (==) (getAnswerUuid formValue.value)) questionData.answers of
                                Just answer ->
                                    QuestionnaireChanges.foldMap (getQuestionChanges appState migration chapter) (getFollowUpQuestions answer)

                                Nothing ->
                                    QuestionnaireChanges.empty

                        ListQuestion questionData ->
                            QuestionnaireChanges.foldMap (getQuestionChanges appState migration chapter) questionData.itemTemplateQuestions

                        _ ->
                            QuestionnaireChanges.empty

                _ ->
                    QuestionnaireChanges.empty
    in
    QuestionnaireChanges.merge questionChange childChanges


getReply : QuestionnaireDetail -> Question -> Maybe FormValue
getReply questionnaire question =
    List.find (.path >> getUuidFromPath >> (==) (getQuestionUuid question)) questionnaire.replies


isNew : QuestionnaireDetail -> Question -> Bool
isNew questionnaire question =
    not <| List.any (getQuestionUuid >> (==) (getQuestionUuid question)) (getQuestions questionnaire.knowledgeModel)


isChanged : Bool -> Question -> Question -> Bool
isChanged levelsEnabled oldQuestion newQuestion =
    (getQuestionTitle oldQuestion /= getQuestionTitle newQuestion)
        || areQuestionDetailsChanged levelsEnabled oldQuestion newQuestion


areQuestionDetailsChanged : Bool -> Question -> Question -> Bool
areQuestionDetailsChanged levelsEnabled oldQuestion newQuestion =
    (getQuestionText oldQuestion /= getQuestionText newQuestion)
        || (levelsEnabled && (getQuestionRequiredLevel oldQuestion /= getQuestionRequiredLevel newQuestion))
        || (getQuestionReferences oldQuestion /= getQuestionReferences newQuestion)
        || (getQuestionExperts oldQuestion /= getQuestionExperts newQuestion)


getAnswerChanges : Question -> Question -> List AnswerChange
getAnswerChanges oldQuestion newQuestion =
    let
        createAnswerChange answer =
            case List.find (.uuid >> (==) answer.uuid) (getQuestionAnswers oldQuestion) of
                Just oldAnswer ->
                    if oldAnswer.label /= answer.label then
                        Just <| AnswerChange <| AnswerChangeData answer oldAnswer

                    else
                        Nothing

                Nothing ->
                    Just <| AnswerAdd <| AnswerAddData answer
    in
    listFilterJust <| List.map createAnswerChange <| getQuestionAnswers newQuestion


getUuidFromPath : String -> String
getUuidFromPath path =
    String.split "." path
        |> List.last
        |> Maybe.withDefault ""


isQuestionChangeResolved : QuestionnaireMigration -> QuestionChange -> Bool
isQuestionChangeResolved migration change =
    List.member (QuestionChange.getQuestionUuid change) migration.resolvedQuestionUuids
