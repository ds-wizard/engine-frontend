module Wizard.Pages.Projects.Migration.View exposing (view)

import Common.Components.FontAwesome exposing (faQuestionnaireMigrationResolve, faQuestionnaireMigrationResolveAll, faQuestionnaireMigrationUndo)
import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Common.Utils.Bool as Bool
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, button, code, div, h5, p, small, strong, table, td, text, th, tr)
import Html.Attributes exposing (class, classList, style, target)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import String.Format as String
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.DiffQuestionnaireRenderer as DiffQuestionnaireRenderer
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Wizard.Pages.Projects.Migration.Models exposing (Model, isQuestionChangeResolved, isSelectedChangeResolved)
import Wizard.Pages.Projects.Migration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.questionnaireMigration


contentView : AppState -> Model -> QuestionnaireMigration -> Html Msg
contentView appState model migration =
    let
        finalizeAction =
            if allResolved model migration then
                button [ class "btn btn-primary", onClick FinalizeMigration, dataCy "project-migration_finalize" ]
                    [ text (gettext "Finalize migration" appState.locale) ]

            else
                Html.nothing

        content =
            if List.isEmpty model.changes.questions then
                div [ class "content" ]
                    [ Page.illustratedMessage
                        { illustration = Undraw.happyFeeling
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
                [ th [] [ text (gettext "Source knowledge model" appState.locale) ]
                , td [] [ packageInfo migration.oldQuestionnaire.knowledgeModelPackageId ]
                ]
            , tr []
                [ th [] [ text (gettext "Target knowledge model" appState.locale) ]
                , td [] [ packageInfo migration.newQuestionnaire.knowledgeModelPackageId ]
                ]
            ]
        ]


packageInfo : String -> Html Msg
packageInfo kmPackageId =
    code []
        [ linkTo (Routes.knowledgeModelsDetail kmPackageId)
            [ target "_blank" ]
            [ text kmPackageId ]
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
                    , button [ class "btn btn-outline-secondary with-icon", onClick UndoResolveCurrentChange, dataCy "project-migration_undo" ]
                        [ faQuestionnaireMigrationUndo, text (gettext "Undo" appState.locale) ]
                    ]

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveCurrentChange, dataCy "project-migration_resolve" ]
                    [ faQuestionnaireMigrationResolve, text (gettext "Resolve" appState.locale) ]

        resolveAllAction =
            if allResolved model migration then
                Html.nothing

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveAllChanges, dataCy "project-migration_resolve-all" ]
                    [ faQuestionnaireMigrationResolveAll, text (gettext "Resolve all" appState.locale) ]
    in
    div [ class "change-view" ]
        [ div [ class "progress-view" ]
            [ text <| String.format (gettext "Resolved changes %s/%s" appState.locale) [ String.fromInt resolvedCount, String.fromInt changesCount ]
            ]
        , div [ class "controls-view d-flex" ]
            [ resolveAction, resolveAllAction ]
        , div [ class "progress mt-2" ]
            [ div [ class "progress-bar", classList [ ( "bg-success", resolvedCount == changesCount ) ], style "width" (progress ++ "%") ] [] ]
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
                    , questionLinksEnabled = False
                    }
                , renderer = DiffQuestionnaireRenderer.create appState migration model.changes migration.newQuestionnaire.knowledgeModel model.selectedChange
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                , isKmEditor = False
                }
                { events = []
                , kmEditorUuid = Nothing
                }
                questionnaireModel

        Nothing ->
            Html.nothing


viewChanges : AppState -> Model -> QuestionnaireMigration -> Html Msg
viewChanges appState model migration =
    div [ class "list-group" ]
        (List.map (viewChange appState model migration) <| List.sortBy (Bool.toInt << isQuestionChangeResolved migration) model.changes.questions)


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
                Html.nothing
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
