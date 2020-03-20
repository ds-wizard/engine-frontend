module Wizard.Settings.Info.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.InfoConfigForm as InfoConfigForm exposing (InfoConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableInfoConfig InfoConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel InfoConfigForm.initEmpty
