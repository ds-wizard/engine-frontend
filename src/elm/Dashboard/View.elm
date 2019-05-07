module Dashboard.View exposing (view)

import Common.AppState as AppState exposing (AppState)
import Common.Config exposing (Widget(..))
import Common.Html exposing (emptyNode)
import Dashboard.Models exposing (Model)
import Dashboard.Widgets.DMPWorkflowWidget as DMPWorkflowWidget
import Dashboard.Widgets.PhaseQuestionnaireWidget as PhaseQuestionnaireWidget
import Dashboard.Widgets.WelcomeWidget as WelcomeWidget
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Markdown


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
            PhaseQuestionnaireWidget.view model.levels model.questionnaires

        Welcome ->
            WelcomeWidget.view appState
