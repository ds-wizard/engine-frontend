module Wizard.Dashboard.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Announcements as Announcements
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
                    DataStewardDashboard.view appState model.dataStewardDashboardModel

                AdminDashboard ->
                    AdminDashboard.view appState model.adminDashboardModel
    in
    div [ class "Dashboard" ]
        [ Announcements.viewDashboard appState.config.dashboardAndLoginScreen.announcements
        , content
        ]
