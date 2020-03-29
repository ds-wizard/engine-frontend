module Wizard.Settings.KnowledgeModelRegistry.Models exposing (..)

import Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig as EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableKnowledgeModelRegistryConfig


initialModel : Model
initialModel =
    GenericModel.initialModel EditableKnowledgeModelRegistryConfig.initEmptyForm
