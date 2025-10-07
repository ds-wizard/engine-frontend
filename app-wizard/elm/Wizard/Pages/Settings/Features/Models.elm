module Wizard.Pages.Settings.Features.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Api.Models.EditableConfig.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableFeaturesConfig


initialModel : Model
initialModel =
    GenericModel.initialModel EditableFeaturesConfig.initEmptyForm
