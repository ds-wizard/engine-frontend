module Wizard.Projects.Migration.Models exposing
    ( Model
    , areQuestionDetailsChanged
    , initialModel
    , initializeChangeList
    , isQuestionChangeResolved
    , isSelectedChangeResolved
    )

import ActionResult exposing (ActionResult(..))
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel, ParentMap)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.ReplyValue as ReplyValue exposing (ReplyValue)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Utils exposing (flip, listFilterJust)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Projects.Common.AnswerChange exposing (AnswerAddData, AnswerChange(..), AnswerChangeData)
import Wizard.Projects.Common.QuestionChange as QuestionChange exposing (QuestionAddData, QuestionChange(..), QuestionChangeData, QuestionMoveData)
import Wizard.Projects.Common.QuestionnaireChanges as QuestionnaireChanges exposing (QuestionnaireChanges)


type alias Model =
    { questionnaireUuid : Uuid
    , questionnaireMigration : ActionResult QuestionnaireMigration
    , levels : ActionResult (List Level)
    , changes : QuestionnaireChanges
    , selectedChange : Maybe QuestionChange
    , questionnaireModel : Maybe Questionnaire.Model
    }


initialModel : Uuid -> Model
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
                context =
                    { oldKM = migration.oldQuestionnaire.knowledgeModel
                    , newKM = migration.newQuestionnaire.knowledgeModel
                    , oldKmParentMap = KnowledgeModel.createParentMap migration.oldQuestionnaire.knowledgeModel
                    , newKmParentMap = KnowledgeModel.createParentMap migration.newQuestionnaire.knowledgeModel
                    , oldQuestionnaire = migration.oldQuestionnaire
                    , newQuestionnaire = migration.newQuestionnaire
                    }

                changes =
                    getChangeList appState context

                selectedChange =
                    changes.questions
                        |> List.filter (QuestionChange.getQuestionUuid >> flip QuestionnaireMigration.isQuestionResolved migration >> not)
                        |> List.head
                        |> Maybe.orElse (List.head changes.questions)
            in
            { model | changes = changes, selectedChange = selectedChange }

        _ ->
            { model | changes = QuestionnaireChanges.empty }


type alias ChangeListContext =
    { oldKM : KnowledgeModel
    , newKM : KnowledgeModel
    , oldKmParentMap : ParentMap
    , newKmParentMap : ParentMap
    , oldQuestionnaire : QuestionnaireDetail
    , newQuestionnaire : QuestionnaireDetail
    }


getChangeList : AppState -> ChangeListContext -> QuestionnaireChanges
getChangeList appState context =
    KnowledgeModel.getChapters context.newKM
        |> QuestionnaireChanges.foldMap (getChapterChanges appState context)


getChapterChanges : AppState -> ChangeListContext -> Chapter -> QuestionnaireChanges
getChapterChanges appState context chapter =
    QuestionnaireChanges.foldMap
        (getQuestionChanges appState context chapter)
        (KnowledgeModel.getChapterQuestions chapter.uuid context.newKM)


getQuestionChanges : AppState -> ChangeListContext -> Chapter -> Question -> QuestionnaireChanges
getQuestionChanges appState context chapter question =
    let
        questionChange =
            case KnowledgeModel.getQuestion (Question.getUuid question) context.oldKM of
                Just oldQuestion ->
                    let
                        answerChanges =
                            getAnswerChanges context question
                    in
                    if not (List.isEmpty answerChanges) || isChanged appState.config.questionnaire.levels.enabled oldQuestion question then
                        QuestionnaireChanges [ QuestionChange <| QuestionChangeData question oldQuestion chapter ] answerChanges

                    else if isMoved context question then
                        QuestionnaireChanges [ QuestionMove <| QuestionMoveData question chapter ] []

                    else
                        QuestionnaireChanges.empty

                Nothing ->
                    if isNew context.oldQuestionnaire question then
                        QuestionnaireChanges [ QuestionAdd <| QuestionAddData question chapter ] []

                    else
                        QuestionnaireChanges.empty

        childChanges =
            case getReply context.newQuestionnaire question of
                Just replyValue ->
                    case question of
                        OptionsQuestion _ _ ->
                            case KnowledgeModel.getAnswer (ReplyValue.getAnswerUuid replyValue) context.newKM of
                                Just answer ->
                                    QuestionnaireChanges.foldMap
                                        (getQuestionChanges appState context chapter)
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid context.newKM)

                                Nothing ->
                                    QuestionnaireChanges.empty

                        ListQuestion commonData _ ->
                            QuestionnaireChanges.foldMap
                                (getQuestionChanges appState context chapter)
                                (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid context.newKM)

                        _ ->
                            QuestionnaireChanges.empty

                _ ->
                    QuestionnaireChanges.empty
    in
    QuestionnaireChanges.merge questionChange childChanges


getReply : QuestionnaireDetail -> Question -> Maybe ReplyValue
getReply questionnaire question =
    Dict.toList questionnaire.replies
        |> List.find (Tuple.first >> getUuidFromPath >> (==) (Question.getUuid question))
        |> Maybe.map Tuple.second


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


isMoved : ChangeListContext -> Question -> Bool
isMoved context question =
    let
        oldParent =
            KnowledgeModel.getParent context.oldKmParentMap <| Question.getUuid question

        newParent =
            KnowledgeModel.getParent context.newKmParentMap <| Question.getUuid question
    in
    oldParent /= newParent


getAnswerChanges : ChangeListContext -> Question -> List AnswerChange
getAnswerChanges context newQuestion =
    let
        createAnswerChange answer =
            case KnowledgeModel.getAnswer answer.uuid context.oldKM of
                Just oldAnswer ->
                    if oldAnswer.label /= answer.label then
                        Just <| AnswerChange <| AnswerChangeData answer oldAnswer

                    else
                        Nothing

                Nothing ->
                    Just <| AnswerAdd <| AnswerAddData answer
    in
    KnowledgeModel.getQuestionAnswers (Question.getUuid newQuestion) context.newKM
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
