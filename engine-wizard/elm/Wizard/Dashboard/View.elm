module Wizard.Dashboard.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Markdown
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Config exposing (Widget(..))
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Dashboard.Models exposing (Model)
import Wizard.Dashboard.Widgets.DMPWorkflowWidget as DMPWorkflowWidget
import Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget as PhaseQuestionnaireWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


view : AppState -> Model -> Html msg
view appState model =
    let
        widgets =
            AppState.getDashboardWidgets appState
                |> List.map (viewWidget appState model)
    in
    div [ class "col Dashboard" ]
        ([ viewAlert "warning" appState.config.client.welcomeWarning
         , viewAlert "info" appState.config.client.welcomeInfo
         ]
            ++ widgets
        )


viewAlert : String -> Maybe String -> Html msg
viewAlert alertClass mbMessage =
    case mbMessage of
        Just message ->
            div [ class <| "alert alert-" ++ alertClass ]
                [ Markdown.toHtml [] message ]

        Nothing ->
            emptyNode


viewWidget : AppState -> Model -> Widget -> Html msg
viewWidget appState model widget =
    case widget of
        DMPWorkflow ->
            DMPWorkflowWidget.view appState model.questionnaires

        LevelsQuestionnaire ->
            PhaseQuestionnaireWidget.view appState model.levels model.questionnaires

        Welcome ->
            WelcomeWidget.view appState
