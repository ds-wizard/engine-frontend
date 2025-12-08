module Wizard.Pages.Projects.Migration.Models exposing
    ( Model
    , areQuestionDetailsChanged
    , initialModel
    , initializeChangeList
    , isQuestionChangeResolved
    , isSelectedChangeResolved
    )

import ActionResult exposing (ActionResult(..))
import Dict
import Flip exposing (flip)
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel, ParentMap)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue)
import Wizard.Api.Models.ProjectMigration as ProjectMigration exposing (ProjectMigration)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Pages.Projects.Common.AnswerChange exposing (AnswerAddData, AnswerChange(..), AnswerChangeData)
import Wizard.Pages.Projects.Common.ChoiceChange exposing (ChoiceAddData, ChoiceChange(..), ChoiceChangeData)
import Wizard.Pages.Projects.Common.QuestionChange as QuestionChange exposing (QuestionAddData, QuestionChange(..), QuestionChangeData, QuestionMoveData)
import Wizard.Pages.Projects.Common.QuestionnaireChanges as QuestionnaireChanges exposing (QuestionnaireChanges)


type alias Model =
    { projectUuid : Uuid
    , projectMigration : ActionResult ProjectMigration
    , changes : QuestionnaireChanges
    , selectedChange : Maybe QuestionChange
    , questionnaireModel : Maybe Questionnaire.Model
    }


initialModel : Uuid -> Model
initialModel projectUuid =
    { projectUuid = projectUuid
    , projectMigration = Loading
    , changes = QuestionnaireChanges.empty
    , selectedChange = Nothing
    , questionnaireModel = Nothing
    }


isSelectedChangeResolved : Model -> Bool
isSelectedChangeResolved model =
    let
        isResolved questionUuid =
            model.projectMigration
                |> ActionResult.map (ProjectMigration.isQuestionResolved questionUuid)
                |> ActionResult.withDefault False
    in
    model.selectedChange
        |> Maybe.map (QuestionChange.getQuestionUuid >> isResolved)
        |> Maybe.withDefault False


initializeChangeList : Model -> Model
initializeChangeList model =
    case model.projectMigration of
        Success migration ->
            let
                context =
                    { oldKM = migration.oldProject.knowledgeModel
                    , newKM = migration.newProject.knowledgeModel
                    , oldKmParentMap = KnowledgeModel.createParentMap migration.oldProject.knowledgeModel
                    , newKmParentMap = KnowledgeModel.createParentMap migration.newProject.knowledgeModel
                    , oldQuestionnaire = migration.oldProject
                    , newQuestionnaire = migration.newProject
                    }

                changes =
                    getChangeList context

                selectedChange =
                    changes.questions
                        |> List.filter (QuestionChange.getQuestionUuid >> flip ProjectMigration.isQuestionResolved migration >> not)
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
    , oldQuestionnaire : ProjectQuestionnaire
    , newQuestionnaire : ProjectQuestionnaire
    }


getChangeList : ChangeListContext -> QuestionnaireChanges
getChangeList context =
    KnowledgeModel.getChapters context.newKM
        |> QuestionnaireChanges.foldMap (getChapterChanges context)


getChapterChanges : ChangeListContext -> Chapter -> QuestionnaireChanges
getChapterChanges context chapter =
    QuestionnaireChanges.foldMap
        (getQuestionChanges context chapter)
        (KnowledgeModel.getChapterQuestions chapter.uuid context.newKM)


getQuestionChanges : ChangeListContext -> Chapter -> Question -> QuestionnaireChanges
getQuestionChanges context chapter question =
    let
        questionChange =
            case KnowledgeModel.getQuestion (Question.getUuid question) context.oldKM of
                Just oldQuestion ->
                    let
                        answerChanges =
                            getAnswerChanges context question

                        choiceChanges =
                            getChoiceChanges context question
                    in
                    if not (List.isEmpty answerChanges) || not (List.isEmpty choiceChanges) || isChanged oldQuestion question then
                        QuestionnaireChanges
                            [ QuestionChange <| QuestionChangeData question oldQuestion chapter ]
                            answerChanges
                            choiceChanges

                    else if isMoved context question then
                        QuestionnaireChanges [ QuestionMove <| QuestionMoveData question chapter ] [] []

                    else
                        QuestionnaireChanges.empty

                Nothing ->
                    if isNew context.oldQuestionnaire question then
                        QuestionnaireChanges [ QuestionAdd <| QuestionAddData question chapter ] [] []

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
                                        (getQuestionChanges context chapter)
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid context.newKM)

                                Nothing ->
                                    QuestionnaireChanges.empty

                        ListQuestion commonData _ ->
                            QuestionnaireChanges.foldMap
                                (getQuestionChanges context chapter)
                                (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid context.newKM)

                        _ ->
                            QuestionnaireChanges.empty

                _ ->
                    QuestionnaireChanges.empty
    in
    QuestionnaireChanges.merge questionChange childChanges


getReply : ProjectQuestionnaire -> Question -> Maybe ReplyValue
getReply questionnaire question =
    Dict.toList questionnaire.replies
        |> List.find (Tuple.first >> getUuidFromPath >> (==) (Question.getUuid question))
        |> Maybe.map (Tuple.second >> .value)


isNew : ProjectQuestionnaire -> Question -> Bool
isNew questionnaire question =
    Maybe.isNothing <| KnowledgeModel.getQuestion (Question.getUuid question) questionnaire.knowledgeModel


isChanged : Question -> Question -> Bool
isChanged oldQuestion newQuestion =
    (Question.getTitle oldQuestion /= Question.getTitle newQuestion)
        || areQuestionDetailsChanged oldQuestion newQuestion


areQuestionDetailsChanged : Question -> Question -> Bool
areQuestionDetailsChanged oldQuestion newQuestion =
    (Question.getText oldQuestion /= Question.getText newQuestion)
        || (Question.getRequiredPhaseUuid oldQuestion /= Question.getRequiredPhaseUuid newQuestion)
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
        |> List.filterMap createAnswerChange


getChoiceChanges : ChangeListContext -> Question -> List ChoiceChange
getChoiceChanges context newQuestion =
    let
        createChoiceChange choice =
            case KnowledgeModel.getChoice choice.uuid context.oldKM of
                Just oldChoice ->
                    if oldChoice.label /= choice.label then
                        Just <| ChoiceChange <| ChoiceChangeData choice oldChoice

                    else
                        Nothing

                Nothing ->
                    Just <| ChoiceAdd <| ChoiceAddData choice
    in
    KnowledgeModel.getQuestionChoices (Question.getUuid newQuestion) context.newKM
        |> List.filterMap createChoiceChange


getUuidFromPath : String -> String
getUuidFromPath path =
    String.split "." path
        |> List.last
        |> Maybe.withDefault ""


isQuestionChangeResolved : ProjectMigration -> QuestionChange -> Bool
isQuestionChangeResolved migration change =
    List.member (QuestionChange.getQuestionUuid change) migration.resolvedQuestionUuids
