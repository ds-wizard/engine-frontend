module Wizard.Dashboard.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)
import Shared.Markdown as Markdown
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard
import Wizard.Dashboard.Dashboards.WelcomeDashboard as WelcomeDashboard
import Wizard.Dashboard.Models exposing (CurrentDashboard(..), Model)
import Wizard.Dashboard.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.currentDashboard of
                WelcomeDashboard ->
                    WelcomeDashboard.view appState

                ResearcherDashboard ->
                    ResearcherDashboard.view appState model.researcherDashboardModel

                DataStewardDashboard ->
                    DataStewardDashboard.view appState

                AdminDashboard ->
                    AdminDashboard.view appState model.adminDashboardModel
    in
    div [ class "Dashboard" ]
        [ viewAlert "warning" appState.config.dashboard.welcomeWarning
        , viewAlert "info" appState.config.dashboard.welcomeInfo
        , content
        ]


viewAlert : String -> Maybe String -> Html Msg
viewAlert alertClass mbMessage =
    case mbMessage of
        Just message ->
            div [ class <| "alert alert-" ++ alertClass, dataCy ("dashboard_alert-" ++ alertClass) ]
                [ Markdown.toHtml [] message ]

        Nothing ->
            emptyNode
