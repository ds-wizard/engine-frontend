module Wizard.Pages.Dashboard.Msgs exposing (Msg(..))

import Wizard.Pages.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Pages.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Pages.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard


type Msg
    = ResearcherDashboardMsg ResearcherDashboard.Msg
    | DataStewardDashboardMsg DataStewardDashboard.Msg
    | AdminDashboardMsg AdminDashboard.Msg
