module Questionnaires.Migration.DiffQuestionnaireRenderer exposing (diffQuestionnaireRenderer)

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (..)
import Common.Questionnaire.Msgs exposing (CustomFormMessage)
import Diff
import FormEngine.View exposing (FormRenderer)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import KMEditor.Common.Models.Entities exposing (Answer, Level, Metric, Question, getQuestionExperts, getQuestionReferences, getQuestionRequiredLevel, getQuestionText, getQuestionTitle, getQuestionUuid)
import List.Extra as List
import Questionnaires.Common.AnswerChange as AnswerChange exposing (AnswerChange(..))
import Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Questionnaires.Common.QuestionnaireChanges exposing (QuestionnaireChanges)
import Questionnaires.Migration.Models exposing (areQuestionDetailsChanged)


diffQuestionnaireRenderer : QuestionnaireChanges -> List Level -> List Metric -> FormRenderer CustomFormMessage Question Answer ApiError
diffQuestionnaireRenderer changes levels metrics =
    { renderQuestionLabel = renderQuestionLabelDiff changes.questions
    , renderQuestionDescription = renderQuestionDescriptionDiff changes.questions levels
    , renderOptionLabel = renderOptionLabelDiff changes.answers
    , renderOptionBadges = renderOptionBadges metrics
    , renderOptionAdvice = renderOptionAdvice
    }


renderQuestionLabelDiff : List QuestionChange -> Question -> Html msg
renderQuestionLabelDiff changes question =
    let
        mbChange =
            List.find (QuestionChange.getQuestionUuid >> (==) (getQuestionUuid question)) changes
    in
    case mbChange of
        Just change ->
            case change of
                QuestionAdd data ->
                    renderQuestionAdd data.question

                QuestionChange data ->
                    renderQuestionChange data.originalQuestion data.question

        Nothing ->
            text <| getQuestionTitle question


renderQuestionDescriptionDiff : List QuestionChange -> List Level -> Question -> Html msg
renderQuestionDescriptionDiff changes levels question =
    let
        mbChange =
            List.find (QuestionChange.getQuestionUuid >> (==) (getQuestionUuid question)) changes
    in
    case mbChange of
        Just change ->
            case change of
                QuestionAdd data ->
                    div [ class "diff" ]
                        [ div [ class "diff-added" ] [ renderQuestionDescription levels question ]
                        ]

                QuestionChange data ->
                    if areQuestionDetailsChanged True data.originalQuestion data.question then
                        div [ class "diff" ]
                            [ div [ class "diff-removed" ] [ renderQuestionDescription levels data.originalQuestion ]
                            , div [ class "diff-added" ] [ renderQuestionDescription levels question ]
                            ]

                    else
                        renderQuestionDescription levels question

        Nothing ->
            renderQuestionDescription levels question


renderOptionLabelDiff : List AnswerChange -> Answer -> Html msg
renderOptionLabelDiff changes answer =
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
    String.split "" << getQuestionTitle


renderAnswerAdd : Answer -> Html msg
renderAnswerAdd question =
    renderDiff <|
        Diff.diff [] (getAnswerDiffableTitle question)


renderAnswerChange : Answer -> Answer -> Html msg
renderAnswerChange original new =
    renderDiff <|
        Diff.diff
            (getAnswerDiffableTitle original)
            (getAnswerDiffableTitle new)


getAnswerDiffableTitle : Answer -> List String
getAnswerDiffableTitle =
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
