module Wizard.Pages.Settings.KnowledgeModels.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Common.Forms.EditableKnowledgeModelConfigFrom as EditableKnowledgeModelConfigForm exposing (EditableKnowledgeModelConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate
import Wizard.Pages.Settings.KnowledgeModels.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableKnowledgeModelConfigForm
updateProps =
    { initForm = .knowledgeModel >> EditableKnowledgeModelConfigForm.init
    , formToConfig = EditableKnowledgeModelConfigForm.toEditableKnowledgeModelConfig >> EditableConfig.updateKnowledgeModel
    , formValidation = EditableKnowledgeModelConfigForm.validation
    }
