module Wizard.Settings.Features.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.FeaturesConfigForm as FeaturesConfigForm exposing (FeaturesConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableFeaturesConfig FeaturesConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel FeaturesConfigForm.initEmpty
