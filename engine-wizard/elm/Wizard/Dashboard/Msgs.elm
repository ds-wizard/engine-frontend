module Wizard.Dashboard.Msgs exposing (Msg(..))

import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard


type Msg
    = ResearcherDashboardMsg ResearcherDashboard.Msg
    | AdminDashboardMsg AdminDashboard.Msg
