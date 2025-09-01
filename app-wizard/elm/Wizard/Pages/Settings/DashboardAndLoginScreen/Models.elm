module Wizard.Pages.Settings.DashboardAndLoginScreen.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Pages.Settings.Common.Forms.DashboardAndLoginScreenConfigForm as DashboardAndLoginScreenConfigForm exposing (DashboardAndLoginScreenConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model DashboardAndLoginScreenConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel DashboardAndLoginScreenConfigForm.initEmpty
