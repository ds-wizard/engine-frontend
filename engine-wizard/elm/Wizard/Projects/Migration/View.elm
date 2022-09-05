module Wizard.Projects.Migration.View exposing (view)

import Html exposing (Html, button, code, div, h5, p, small, strong, table, td, text, th, tr)
import Html.Attributes exposing (class, classList, style, target)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.Package exposing (Package)
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lf, lx)
import Shared.Undraw as Undraw
import Shared.Utils exposing (boolToInt, flip)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DiffQuestionnaireRenderer as DiffQuestionnaireRenderer
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange(..))
import Wizard.Projects.Migration.Models exposing (Model, isQuestionChangeResolved, isSelectedChangeResolved)
import Wizard.Projects.Migration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Migration.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Migration.View"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Projects.Migration.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.questionnaireMigration


contentView : AppState -> Model -> QuestionnaireMigration -> Html Msg
contentView appState model migration =
    let
        finalizeAction =
            if allResolved model migration then
                button [ class "btn btn-primary", onClick FinalizeMigration ]
                    [ lx_ "navbar.finalize" appState ]

            else
                emptyNode

        content =
            if List.length model.changes.questions == 0 then
                div [ class "content" ]
                    [ Page.illustratedMessage
                        { image = Undraw.happyFeeling
                        , heading = l_ "noChanges.heading" appState
                        , lines =
                            [ l_ "noChanges.line1" appState
                            , l_ "noChanges.line2" appState
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
                [ th [] [ lx_ "navbar.sourceKM" appState ]
                , td [] [ packageInfo appState migration.oldQuestionnaire.package ]
                ]
            , tr []
                [ th [] [ lx_ "navbar.targetKM" appState ]
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
                    [ lx_ "changeView.resolved" appState
                    , button [ class "btn btn-outline-secondary with-icon", onClick UndoResolveCurrentChange ]
                        [ faSet "questionnaireMigration.undo" appState, lx_ "changeView.undo" appState ]
                    ]

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveCurrentChange ]
                    [ faSet "questionnaireMigration.resolve" appState, lx_ "changeView.resolve" appState ]

        resolveAllAction =
            if allResolved model migration then
                emptyNode

            else
                button [ class "btn btn-outline-primary with-icon", onClick ResolveAllChanges ]
                    [ faSet "questionnaireMigration.resolveAll" appState, lx_ "changeView.resolveAll" appState ]
    in
    div [ class "change-view" ]
        [ div [ class "progress-view" ]
            [ text <| lf_ "changeView.resolvedChanges" [ String.fromInt resolvedCount, String.fromInt changesCount ] appState
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
                    ( l_ "change.questionAdd" appState, data.question )

                QuestionChange data ->
                    ( l_ "change.questionChange" appState, data.question )

                QuestionMove data ->
                    ( l_ "change.questionMove" appState, data.question )

        resolvedLabel =
            if isQuestionChangeResolved migration change then
                small [] [ lx_ "change.resolved" appState ]

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
