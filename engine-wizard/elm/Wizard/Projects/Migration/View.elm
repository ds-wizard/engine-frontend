module Wizard.Projects.Migration.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, button, code, div, h5, p, small, strong, table, td, text, th, tr)
import Html.Attributes exposing (class, classList, style, target)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.Package exposing (Package)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Undraw as Undraw
import Shared.Utils exposing (boolToInt, flip)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DiffQuestionnaireRenderer as DiffQuestionnaireRenderer
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Wizard.Projects.Migration.Models exposing (Model, isQuestionChangeResolved, isSelectedChangeResolved)
import Wizard.Projects.Migration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.questionnaireMigration


contentView : AppState -> Model -> QuestionnaireMigration -> Html Msg
contentView appState model migration =
    let
        finalizeAction =
            if allResolved model migration then
                button [ class "btn btn-primary", onClick FinalizeMigration ]
                    [ text (gettext "Finalize migration" appState.locale) ]

            else
                emptyNode

        content =
            if List.length model.changes.questions == 0 then
                div [ class "content" ]
                    [ Page.illustratedMessage
                        { image = Undraw.happyFeeling
                        , heading = gettext "No changes to review" appState.locale
                        , lines =
                            [ gettext "There are no changes affecting your answers." appState.locale
                            , gettext "You can safely finalize the migration." appState.locale
                            ]
                        , cy = "no-changes"
                        }
                    ]

            else
                div [ class "content" ]
                    [ div [ class "changes-view" ]
                        [ viewChanges appState model migration
                        ]
                    , div [ class "right-view" ]
                        [ changeView appState model migration
                        , div [ class "questionnaire-view" ]
                            [ questionnaireView appState model migration ]
                        ]
                    ]
    in
    div [ class "Questionnaire__Migration" ]
        [ div [ class "top-header" ]
            [ div [ class "top-header-content" ]
                [ div [ class "top-header-title" ]
                    [ migrationInfo appState migration ]
                , div [ class "top-header-actions" ]
                    [ finalizeAction ]
                ]
            ]
        , content
        ]


migrationInfo : AppState -> QuestionnaireMigration -> Html Msg
migrationInfo appState migration =
    div [ class "migration-info" ]
        [ strong [] [ text migration.newQuestionnaire.name ]
        , table []
            [ tr []
                [ th [] [ text (gettext "Source KM" appState.locale) ]
                , td [] [ packageInfo appState migration.oldQuestionnaire.package ]
                ]
            , tr []
                [ th [] [ text (gettext "Target KM" appState.locale) ]
                , td [] [ packageInfo appState migration.newQuestionnaire.package ]
                ]
            ]
        ]


packageInfo : AppState -> Package -> Html Msg
packageInfo appState package =
    code []
        [ linkTo appState
            (Routes.knowledgeModelsDetail package.id)
            [ target "_blank" ]
            [ text package.id ]
        ]


changeView : AppState -> Model -> QuestionnaireMigration -> Html Msg
changeView appState model migration =
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
                    [ text (gettext "Change already resolved" appState.locale)
                    , button [ class "btn btn-outline-secondary with-icon", onClick UndoResolveCurrentChange ]
                        [ faSet "questionnaireMigration.undo" appState, text (gettext "Undo" appState.locale) ]
                    ]

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveCurrentChange ]
                    [ faSet "questionnaireMigration.resolve" appState, text (gettext "Resolve" appState.locale) ]

        resolveAllAction =
            if allResolved model migration then
                emptyNode

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveAllChanges ]
                    [ faSet "questionnaireMigration.resolveAll" appState, text (gettext "Resolve all" appState.locale) ]
    in
    div [ class "change-view" ]
        [ div [ class "progress-view" ]
            [ text <| String.format (gettext "Resolved changes %s/%s" appState.locale) [ String.fromInt resolvedCount, String.fromInt changesCount ]
            , div [ class "progress" ]
                [ div [ class "progress-bar", classList [ ( "bg-success", resolvedCount == changesCount ) ], style "width" (progress ++ "%") ] [] ]
            ]
        , div [ class "controls-view d-flex" ]
            [ resolveAction, resolveAllAction ]
        ]


questionnaireView : AppState -> Model -> QuestionnaireMigration -> Html Msg
questionnaireView appState model migration =
    case model.questionnaireModel of
        Just questionnaireModel ->
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = True
                    , commentsEnabled = False
                    , readonly = True
                    , toolbarEnabled = False
                    }
                , renderer = DiffQuestionnaireRenderer.create appState migration model.changes migration.newQuestionnaire.knowledgeModel model.selectedChange
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                { events = []
                }
                questionnaireModel

        Nothing ->
            emptyNode


viewChanges : AppState -> Model -> QuestionnaireMigration -> Html Msg
viewChanges appState model migration =
    div [ class "list-group" ]
        (List.map (viewChange appState model migration) <| List.sortBy (boolToInt << isQuestionChangeResolved migration) model.changes.questions)


viewChange : AppState -> Model -> QuestionnaireMigration -> QuestionChange -> Html Msg
viewChange appState model migration change =
    let
        ( eventLabel, question ) =
            case change of
                QuestionAdd data ->
                    ( gettext "New Question" appState.locale, data.question )

                QuestionChange data ->
                    ( gettext "Question Changed" appState.locale, data.question )

                QuestionMove data ->
                    ( gettext "Moved Question" appState.locale, data.question )

        resolvedLabel =
            if isQuestionChangeResolved migration change then
                small [] [ text (gettext "Resolved" appState.locale) ]

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


allResolved : Model -> QuestionnaireMigration -> Bool
allResolved model migration =
    model.changes.questions
        |> List.map (QuestionChange.getQuestionUuid >> flip QuestionnaireMigration.isQuestionResolved migration)
        |> List.all ((==) True)
