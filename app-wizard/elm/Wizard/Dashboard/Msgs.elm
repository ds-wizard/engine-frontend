module Wizard.Dashboard.Msgs exposing (Msg(..))

import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard


type Msg
    = ResearcherDashboardMsg ResearcherDashboard.Msg
    | DataStewardDashboardMsg DataStewardDashboard.Msg
    | AdminDashboardMsg AdminDashboard.Msg
