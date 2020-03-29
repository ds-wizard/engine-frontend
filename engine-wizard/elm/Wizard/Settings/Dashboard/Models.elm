module Wizard.Settings.Dashboard.Models exposing (..)

import Wizard.Settings.Common.Forms.DashboardConfigForm as DashboardConfigForm exposing (DashboardConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model DashboardConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel DashboardConfigForm.initEmpty
