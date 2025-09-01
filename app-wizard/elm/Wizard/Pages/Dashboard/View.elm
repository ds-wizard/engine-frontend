module Wizard.Pages.Dashboard.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Components.Announcements as Announcements
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Pages.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Pages.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard
import Wizard.Pages.Dashboard.Dashboards.WelcomeDashboard as WelcomeDashboard
import Wizard.Pages.Dashboard.Models exposing (CurrentDashboard(..), Model)
import Wizard.Pages.Dashboard.Msgs exposing (Msg)


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
