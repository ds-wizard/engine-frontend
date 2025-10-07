module Wizard.Pages.Settings.DashboardAndLoginScreen.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Common.Forms.DashboardAndLoginScreenConfigForm as DashboardAndLoginScreenConfigForm exposing (DashboardAndLoginScreenConfigForm)
import Wizard.Pages.Settings.DashboardAndLoginScreen.Models exposing (Model)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps DashboardAndLoginScreenConfigForm
updateProps =
    { initForm = .dashboardAndLoginScreen >> DashboardAndLoginScreenConfigForm.init
    , formToConfig = DashboardAndLoginScreenConfigForm.toDashboardConfig >> EditableConfig.updateDashboardAndLoginScreen
    , formValidation = DashboardAndLoginScreenConfigForm.validation
    }
