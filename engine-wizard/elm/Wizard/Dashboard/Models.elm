module Wizard.Dashboard.Models exposing
    ( CurrentDashboard(..)
    , Model
    , initialModel
    )

import Shared.Auth.Role as Role
import Shared.Auth.Session as Session
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardType as DashboardType
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard


type alias Model =
    { currentDashboard : CurrentDashboard
    , researcherDashboardModel : ResearcherDashboard.Model
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
            case appState.config.dashboard.dashboardType of
                DashboardType.Welcome ->
                    WelcomeDashboard

                DashboardType.RoleBased ->
                    Role.switch (Session.getUserRole appState.session)
                        AdminDashboard
                        DataStewardDashboard
                        ResearcherDashboard
                        ResearcherDashboard
    in
    { currentDashboard = currentDashboard
    , researcherDashboardModel = ResearcherDashboard.initialModel
    , adminDashboardModel = AdminDashboard.initialModel
    }
