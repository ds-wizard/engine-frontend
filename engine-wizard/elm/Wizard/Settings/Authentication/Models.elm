module Wizard.Settings.Authentication.Models exposing (Model, initialModel)

import Wizard.Settings.Common.Forms.AuthenticationConfigForm as AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model AuthenticationConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel AuthenticationConfigForm.initEmpty
