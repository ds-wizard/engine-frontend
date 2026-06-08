module Wizard.Pages.Dashboard.Models exposing
    ( CurrentDashboard(..)
    , Model
    , initialModel
    )

import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.DashboardType as DashboardType
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Dashboards.WidgetDashboard as WidgetDashboard


type alias Model =
    { currentDashboard : CurrentDashboard
    , widgetDashboardModel : WidgetDashboard.Model
    }


type CurrentDashboard
    = WelcomeDashboard
    | WidgetDashboard


initialModel : AppState -> Model
initialModel appState =
    let
        currentDashboard =
            case appState.config.dashboardAndLoginScreen.dashboardType of
                DashboardType.Welcome ->
                    WelcomeDashboard

                DashboardType.RoleBased ->
                    WidgetDashboard
    in
    { currentDashboard = currentDashboard
    , widgetDashboardModel = WidgetDashboard.initialModel
    }
