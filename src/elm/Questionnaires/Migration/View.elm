module Questionnaires.Migration.View exposing (view)

import ActionResult
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Questionnaire.Models
import Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Question as Question
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Common.Version as Version
import Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Questionnaires.Common.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Questionnaires.Migration.DiffQuestionnaireRenderer exposing (diffQuestionnaireRenderer)
import Questionnaires.Migration.Models exposing (Model, isQuestionChangeResolved, isSelectedChangeResolved)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import Utils exposing (boolToInt, flip)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (contentView appState model) (ActionResult.combine model.questionnaireMigration model.levels)


contentView : AppState -> Model -> ( QuestionnaireMigration, List Level ) -> Html Msg
contentView appState model ( migration, levels ) =
    let
        allResolved =
            model.changes.questions
                |> List.map (QuestionChange.getQuestionUuid >> flip QuestionnaireMigration.isQuestionResolved migration)
                |> List.all ((==) True)

        finalizeAction =
            if allResolved then
                button [ class "btn btn-primary link-with-icon", onClick FinalizeMigration ]
                    [ text "Finalize Migration" ]

            else
                emptyNode

        content =
            if List.length model.changes.questions == 0 then
                div [ class "content" ]
                    [ Page.illustratedMessage
                        { image = "happy_feeling"
                        , heading = "No changes to review"
                        , lines =
                            [ "There are no changes affecting your answers."
                            , "You can safely finalize the migration."
                            ]
                        }
                    ]

            else
                div [ class "content" ]
                    [ div [ class "changes-view" ]
                        [ viewChanges model migration
                        ]
                    , div [ class "right-view" ]
                        [ changeView model migration
                        , div [ class "questionnaire-view" ]
                            [ model.questionnaireModel
                                |> Maybe.map (questionnaireView appState model migration levels)
                                |> Maybe.withDefault emptyNode
                            ]
                        ]
                    ]
    in
    div [ class "Questionnaire__Migration" ]
        [ div [ class "top-header" ]
            [ div [ class "top-header-content" ]
                [ div [ class "top-header-title" ]
                    [ migrationInfo migration ]
                , div [ class "top-header-actions" ]
                    [ finalizeAction ]
                ]
            ]
        , content
        ]


migrationInfo : QuestionnaireMigration -> Html Msg
migrationInfo migration =
    div [ class "migration-info" ]
        [ strong [] [ text migration.newQuestionnaire.name ]
        , div []
            [ text "Migration: "
            , packageInfo migration.oldQuestionnaire.package
            , fa "long-arrow-right"
            , packageInfo migration.newQuestionnaire.package
            ]
        ]


packageInfo : Package -> Html Msg
packageInfo package =
    span [ class "package-info" ]
        [ text <| package.name ++ " (" ++ Version.toString package.version ++ ") "
        , code [] [ text package.id ]
        ]


changeView : Model -> QuestionnaireMigration -> Html Msg
changeView model migration =
    let
        resolvedCount =
            List.length migration.resolvedQuestionUuids

        changesCount =
            List.length model.changes.questions

        progress =
            String.fromFloat <| 100 * toFloat resolvedCount / toFloat changesCount

        resolveAction =
            if isSelectedChangeResolved model then
                div []
                    [ text "Change already resolved"
                    , button [ class "btn btn-outline-secondary link-with-icon", onClick UndoResolveCurrentChange ]
                        [ fa "undo", text "Undo" ]
                    ]

            else
                button [ class "btn btn-outline-primary link-with-icon", onClick ResolveCurrentChange ]
                    [ fa "check", text "Resolve" ]
    in
    div [ class "change-view" ]
        [ div [ class "progress-view" ]
            [ text <| "Resolved changes " ++ String.fromInt resolvedCount ++ "/" ++ String.fromInt changesCount
            , div [ class "progress" ]
                [ div [ class "progress-bar", classList [ ( "bg-success", resolvedCount == changesCount ) ], style "width" (progress ++ "%") ] [] ]
            ]
        , div [ class "controls-view" ]
            [ resolveAction ]
        ]


questionnaireView : AppState -> Model -> QuestionnaireMigration -> List Level -> Common.Questionnaire.Models.Model -> Html Msg
questionnaireView appState model migration levels questionnaireModel =
    let
        getExtraQuestionClass uuid =
            if Just uuid == Maybe.map QuestionChange.getQuestionUuid model.selectedChange then
                if QuestionnaireMigration.isQuestionResolved uuid migration then
                    Just "highlighted highlighted-resolved"

                else
                    Just "highlighted"

            else
                Nothing

        mbLevels =
            if appState.config.levelsEnabled then
                Just levels

            else
                Nothing
    in
    viewQuestionnaire
        { features = [ QuestionnaireFeature.todos ]
        , levels = mbLevels
        , getExtraQuestionClass = getExtraQuestionClass
        , forceDisabled = True
        , createRenderer = diffQuestionnaireRenderer model.changes
        }
        appState
        questionnaireModel
        |> Html.map QuestionnaireMsg


viewChanges : Model -> QuestionnaireMigration -> Html Msg
viewChanges model migration =
    div [ class "list-group" ]
        (List.map (viewChange model migration) <| List.sortBy (boolToInt << isQuestionChangeResolved migration) model.changes.questions)


viewChange : Model -> QuestionnaireMigration -> QuestionChange -> Html Msg
viewChange model migration change =
    let
        ( eventLabel, question ) =
            case change of
                QuestionAdd data ->
                    ( "New Question", data.question )

                QuestionChange data ->
                    ( "Question Changed", data.question )

        resolvedLabel =
            if isQuestionChangeResolved migration change then
                small [] [ text "Resolved" ]

            else
                emptyNode
    in
    div
        [ classList
            [ ( "selected", Just change == model.selectedChange )
            , ( "resolved", isQuestionChangeResolved migration change )
            ]
        , class "list-group-item list-group-item-action flex-column align-items-start"
        , onClick <| SelectChange change
        ]
        [ div [ class "d-flex w-100 justify-content-between" ]
            [ h5 [] [ text eventLabel ]
            , resolvedLabel
            ]
        , p [ class "mb-0" ] [ text <| Question.getTitle question ]
        ]
