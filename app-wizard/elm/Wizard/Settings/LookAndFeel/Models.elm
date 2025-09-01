module Wizard.Settings.LookAndFeel.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Api.Models.EditableConfig.EditableLookAndFeelConfig as EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableLookAndFeelConfig


initialModel : Model
initialModel =
    GenericModel.initialModel EditableLookAndFeelConfig.initEmptyForm
