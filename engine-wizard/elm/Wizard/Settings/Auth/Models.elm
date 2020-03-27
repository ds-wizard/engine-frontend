module Wizard.Settings.Auth.Models exposing (Model, initialModel)

import Wizard.Settings.Common.AuthConfigForm as AuthConfigForm exposing (AuthConfigForm)
import Wizard.Settings.Common.EditableAuthConfig exposing (EditableAuthConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableAuthConfig AuthConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel AuthConfigForm.initEmpty
