module Wizard.Settings.LookAndFeel.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model LookAndFeelConfig


initialModel : Model
initialModel =
    GenericModel.initialModel LookAndFeelConfig.initEmptyForm
