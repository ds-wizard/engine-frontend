module Wizard.Settings.PrivacyAndSupport.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model PrivacyAndSupportConfig


initialModel : Model
initialModel =
    GenericModel.initialModel PrivacyAndSupportConfig.initEmptyForm
