module Wizard.Settings.KnowledgeModels.Update exposing (update)

import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.EditableKnowledgeModelConfigFrom as EditableKnowledgeModelConfigForm exposing (EditableKnowledgeModelConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.KnowledgeModels.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableKnowledgeModelConfigForm
updateProps =
    { initForm = .knowledgeModel >> EditableKnowledgeModelConfigForm.init
    , formToConfig = EditableKnowledgeModelConfigForm.toEditableKnowledgeModelConfig >> EditableConfig.updateKnowledgeModel
    , formValidation = EditableKnowledgeModelConfigForm.validation
    }
