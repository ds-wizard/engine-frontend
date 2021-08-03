module Wizard.Common.Components.Questionnaire.DiffQuestionnaireRenderer exposing (create)

import Diff exposing (Change)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire exposing (QuestionnaireRenderer)
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Projects.Common.AnswerChange as AnswerChange exposing (AnswerChange(..))
import Wizard.Projects.Common.ChoiceChange as ChoiceChange exposing (ChoiceChange(..))
import Wizard.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Wizard.Projects.Common.QuestionnaireChanges exposing (QuestionnaireChanges)
import Wizard.Projects.Migration.Models exposing (areQuestionDetailsChanged)


create : AppState -> QuestionnaireMigration -> QuestionnaireChanges -> KnowledgeModel -> Maybe QuestionChange -> QuestionnaireRenderer msg
create appState migration changes km mbSelectedChange =
    let
        defaultRenderer =
            DefaultQuestionnaireRenderer.create appState km

        getExtraQuestionClass question =
            if Just (Question.getUuid question) == Maybe.map QuestionChange.getQuestionUuid mbSelectedChange then
                if QuestionnaireMigration.isQuestionResolved (Question.getUuid question) migration then
                    Just "highlighted highlighted-resolved"

                else
                    Just "highlighted"

            else
                Nothing
    in
    { renderQuestionLabel = renderQuestionLabelDiff changes.questions
    , renderQuestionDescription = renderQuestionDescriptionDiff defaultRenderer changes.questions
    , getQuestionExtraClass = getExtraQuestionClass
    , renderAnswerLabel = renderAnswerLabelDiff changes.answers
    , renderAnswerBadges = defaultRenderer.renderAnswerBadges
    , renderAnswerAdvice = defaultRenderer.renderAnswerAdvice
    , renderChoiceLabel = renderChoiceLabelDiff changes.choices
    }


renderQuestionLabelDiff : List QuestionChange -> Question -> Html msg
renderQuestionLabelDiff changes question =
    let
        mbChange =
            List.find (QuestionChange.getQuestionUuid >> (==) (Question.getUuid question)) changes
    in
    case mbChange of
        Just change ->
            case change of
                QuestionAdd data ->
                    renderQuestionAdd data.question

                QuestionChange data ->
                    renderQuestionChange data.originalQuestion data.question

                QuestionMove data ->
                    span [] [ text <| Question.getTitle data.question ]

        Nothing ->
            text <| Question.getTitle question


renderQuestionDescriptionDiff : QuestionnaireRenderer msg -> List QuestionChange -> QuestionnaireViewSettings -> Question -> Html msg
renderQuestionDescriptionDiff defaultRenderer changes qvs question =
    let
        mbChange =
            List.find (QuestionChange.getQuestionUuid >> (==) (Question.getUuid question)) changes
    in
    case mbChange of
        Just change ->
            case change of
                QuestionAdd _ ->
                    div [ class "diff" ]
                        [ div [ class "diff-added" ] [ defaultRenderer.renderQuestionDescription qvs question ]
                        ]

                QuestionChange data ->
                    if areQuestionDetailsChanged data.originalQuestion data.question then
                        div [ class "diff" ]
                            [ div [ class "diff-removed" ] [ defaultRenderer.renderQuestionDescription qvs data.originalQuestion ]
                            , div [ class "diff-added" ] [ defaultRenderer.renderQuestionDescription qvs question ]
                            ]

                    else
                        defaultRenderer.renderQuestionDescription qvs question

                QuestionMove _ ->
                    div [ class "diff" ]
                        [ div [] [ defaultRenderer.renderQuestionDescription qvs question ]
                        ]

        Nothing ->
            defaultRenderer.renderQuestionDescription qvs question


renderAnswerLabelDiff : List AnswerChange -> Answer -> Html msg
renderAnswerLabelDiff changes answer =
    let
        mbChange =
            List.find (AnswerChange.getAnswerUuid >> (==) answer.uuid) changes
    in
    case mbChange of
        Just change ->
            case change of
                AnswerAdd data ->
                    renderAnswerAdd data.answer

                AnswerChange data ->
                    renderAnswerChange data.originalAnswer data.answer

        Nothing ->
            text answer.label


renderChoiceLabelDiff : List ChoiceChange -> Choice -> Html msg
renderChoiceLabelDiff changes choice =
    let
        mbChange =
            List.find (ChoiceChange.getChoiceUuid >> (==) choice.uuid) changes
    in
    case mbChange of
        Just change ->
            case change of
                ChoiceAdd data ->
                    renderChoiceAdd data.choice

                ChoiceChange data ->
                    renderChoiceChange data.originalChoice data.choice

        Nothing ->
            text choice.label



-- Diff views


renderQuestionAdd : Question -> Html msg
renderQuestionAdd question =
    renderDiff <|
        Diff.diff [] (getQuestionDiffableTitle question)


renderQuestionChange : Question -> Question -> Html msg
renderQuestionChange original new =
    renderDiff <|
        Diff.diff
            (getQuestionDiffableTitle original)
            (getQuestionDiffableTitle new)


getQuestionDiffableTitle : Question -> List String
getQuestionDiffableTitle =
    String.split "" << Question.getTitle


renderAnswerAdd : Answer -> Html msg
renderAnswerAdd answer =
    renderDiff <|
        Diff.diff [] (getAnswerDiffableTitle answer)


renderAnswerChange : Answer -> Answer -> Html msg
renderAnswerChange original new =
    renderDiff <|
        Diff.diff
            (getAnswerDiffableTitle original)
            (getAnswerDiffableTitle new)


getAnswerDiffableTitle : Answer -> List String
getAnswerDiffableTitle =
    String.split "" << .label


renderChoiceAdd : Choice -> Html msg
renderChoiceAdd choice =
    renderDiff <|
        Diff.diff [] (getChoiceDiffableTitle choice)


renderChoiceChange : Choice -> Choice -> Html msg
renderChoiceChange original new =
    renderDiff <|
        Diff.diff
            (getChoiceDiffableTitle original)
            (getChoiceDiffableTitle new)


getChoiceDiffableTitle : Choice -> List String
getChoiceDiffableTitle =
    String.split "" << .label


renderDiff : List (Diff.Change String) -> Html msg
renderDiff =
    span [ class "diff" ] << List.map renderChange << mergeDiffs


mergeDiffs : List (Diff.Change String) -> List (Diff.Change String)
mergeDiffs list =
    let
        dropLast =
            List.reverse >> List.drop 1 >> List.reverse

        fold item changes =
            case ( List.last changes, item ) of
                ( Just (Diff.Added s1), Diff.Added s2 ) ->
                    dropLast changes ++ [ Diff.Added (s1 ++ s2) ]

                ( Just (Diff.Removed s1), Diff.Removed s2 ) ->
                    dropLast changes ++ [ Diff.Removed (s1 ++ s2) ]

                ( Just (Diff.NoChange s1), Diff.NoChange s2 ) ->
                    dropLast changes ++ [ Diff.NoChange (s1 ++ s2) ]

                _ ->
                    changes ++ [ item ]
    in
    List.foldl fold [] list


renderChange : Diff.Change String -> Html msg
renderChange change =
    case change of
        Diff.Added s ->
            span [ class "diff-added" ] [ text s ]

        Diff.Removed s ->
            span [ class "diff-removed" ] [ text s ]

        Diff.NoChange s ->
            span [ class "diff-unchanged" ] [ text s ]
