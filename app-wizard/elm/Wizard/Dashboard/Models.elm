module Wizard.Dashboard.Models exposing
    ( CurrentDashboard(..)
    , Model
    , initialModel
    )

import Shared.Data.Role as Role
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.DashboardType as DashboardType
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard


type alias Model =
    { currentDashboard : CurrentDashboard
    , researcherDashboardModel : ResearcherDashboard.Model
    , dataStewardDashboardModel : DataStewardDashboard.Model
    , adminDashboardModel : AdminDashboard.Model
    }


type CurrentDashboard
    = WelcomeDashboard
    | ResearcherDashboard
    | DataStewardDashboard
    | AdminDashboard


initialModel : AppState -> Model
initialModel appState =
    let
        currentDashboard =
            case appState.config.dashboardAndLoginScreen.dashboardType of
                DashboardType.Welcome ->
                    WelcomeDashboard

                DashboardType.RoleBased ->
                    case AppState.getUserRole appState of
                        Just role ->
                            Role.switch role
                                AdminDashboard
                                DataStewardDashboard
                                ResearcherDashboard

                        Nothing ->
                            ResearcherDashboard
    in
    { currentDashboard = currentDashboard
    , researcherDashboardModel = ResearcherDashboard.initialModel
    , dataStewardDashboardModel = DataStewardDashboard.initialModel
    , adminDashboardModel = AdminDashboard.initialModel
    }
