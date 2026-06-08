module Wizard.Pages.Dashboard.Msgs exposing (Msg(..))

import Wizard.Pages.Dashboard.Dashboards.WidgetDashboard as WidgetDashboard


type Msg
    = WidgetDashboardMsg WidgetDashboard.Msg
