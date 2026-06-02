module Wizard.Pages.Settings.Authentication.Models exposing (Model, initialModel)

import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model AuthenticationConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel AuthenticationConfigForm.initEmpty
