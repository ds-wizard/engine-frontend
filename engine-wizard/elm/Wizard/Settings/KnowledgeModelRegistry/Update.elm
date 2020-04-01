module Wizard.Settings.KnowledgeModelRegistry.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig as EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.KnowledgeModelRegistry.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableKnowledgeModelRegistryConfig
updateProps =
    { initForm = .knowledgeModelRegistry >> EditableKnowledgeModelRegistryConfig.initForm
    , formToConfig = EditableConfig.updateKnowledgeModelRegistry
    , formValidation = EditableKnowledgeModelRegistryConfig.validation
    }
