module Wizard.Settings.Dashboard.Update exposing (..)

import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.DashboardConfigForm as DashboardConfigForm exposing (DashboardConfigForm)
import Wizard.Settings.Dashboard.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps DashboardConfigForm
updateProps =
    { initForm = .dashboard >> DashboardConfigForm.init
    , formToConfig = DashboardConfigForm.toDashboardConfig >> EditableConfig.updateDashboard
    , formValidation = DashboardConfigForm.validation
    }
