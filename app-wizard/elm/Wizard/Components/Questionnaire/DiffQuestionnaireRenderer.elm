module Wizard.Components.Questionnaire.DiffQuestionnaireRenderer exposing (create)

import Diff
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectMigration as ProjectMigration exposing (ProjectMigration)
import Wizard.Components.Questionnaire as Questionnaire exposing (QuestionnaireRenderer)
import Wizard.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Common.AnswerChange as AnswerChange exposing (AnswerChange(..))
import Wizard.Pages.Projects.Common.ChoiceChange as ChoiceChange exposing (ChoiceChange(..))
import Wizard.Pages.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Wizard.Pages.Projects.Common.QuestionnaireChanges exposing (QuestionnaireChanges)
import Wizard.Pages.Projects.Migration.Models exposing (areQuestionDetailsChanged)


create : AppState -> ProjectMigration -> QuestionnaireChanges -> KnowledgeModel -> Maybe QuestionChange -> QuestionnaireRenderer
create appState migration changes km mbSelectedChange =
    let
        defaultRenderer =
            DefaultQuestionnaireRenderer.create appState
                (DefaultQuestionnaireRenderer.config migration.newProject
                    |> DefaultQuestionnaireRenderer.withKnowledgeModel km
                )

        getExtraQuestionClass question =
            if Just (Question.getUuid question) == Maybe.map QuestionChange.getQuestionUuid mbSelectedChange then
                if ProjectMigration.isQuestionResolved (Question.getUuid question) migration then
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


renderQuestionDescriptionDiff : QuestionnaireRenderer -> List QuestionChange -> QuestionnaireViewSettings -> Question -> Html Questionnaire.Msg
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
