module Wizard.Settings.Client.Models exposing (Model, initialModel)

import Wizard.Settings.Common.ClientConfigForm as ClientConfigForm exposing (ClientConfigForm)
import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableClientConfig ClientConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel ClientConfigForm.initEmpty
