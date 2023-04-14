module Wizard.Settings.DashboardAndLoginScreen.Update exposing (update)

import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.DashboardAndLoginScreenConfigForm as DashboardAndLoginScreenConfigForm exposing (DashboardAndLoginScreenConfigForm)
import Wizard.Settings.DashboardAndLoginScreen.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps DashboardAndLoginScreenConfigForm
updateProps =
    { initForm = .dashboardAndLoginScreen >> DashboardAndLoginScreenConfigForm.init
    , formToConfig = DashboardAndLoginScreenConfigForm.toDashboardConfig >> EditableConfig.updateDashboardAndLoginScreen
    , formValidation = DashboardAndLoginScreenConfigForm.validation
    }
