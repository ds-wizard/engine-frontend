module Wizard.Dashboard.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Markdown
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Dashboard.Models exposing (Model)
import Wizard.Dashboard.Msgs exposing (Msg)
import Wizard.Dashboard.Widgets.DMPWorkflowWidget as DMPWorkflowWidget
import Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget as PhaseQuestionnaireWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


view : AppState -> Model -> Html Msg
view appState model =
    let
        widgets =
            AppState.getDashboardWidgets appState
                |> List.map (viewWidget appState model)
    in
    div [ class "col Dashboard" ]
        ([ viewAlert "warning" appState.config.dashboard.welcomeWarning
         , viewAlert "info" appState.config.dashboard.welcomeInfo
         ]
            ++ widgets
        )


viewAlert : String -> Maybe String -> Html Msg
viewAlert alertClass mbMessage =
    case mbMessage of
        Just message ->
            div [ class <| "alert alert-" ++ alertClass ]
                [ Markdown.toHtml [] message ]

        Nothing ->
            emptyNode


viewWidget : AppState -> Model -> DashboardWidget -> Html Msg
viewWidget appState model widget =
    case widget of
        DMPWorkflowDashboardWidget ->
            DMPWorkflowWidget.view appState model.questionnaires

        LevelsQuestionnaireDashboardWidget ->
            PhaseQuestionnaireWidget.view appState model.questionnaires

        WelcomeDashboardWidget ->
            WelcomeWidget.view appState
