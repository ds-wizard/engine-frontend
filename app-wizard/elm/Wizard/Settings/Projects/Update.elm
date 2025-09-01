module Wizard.Settings.Projects.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Projects.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState =
    GenericUpdate.update (updateProps appState) wrapMsg msg appState


updateProps : AppState -> GenericUpdate.UpdateProps EditableQuestionnairesConfigForm
updateProps appState =
    { initForm = .questionnaires >> EditableQuestionnairesConfigForm.init appState
    , formToConfig = EditableQuestionnairesConfigForm.toEditableQuestionnaireConfig >> EditableConfig.updateQuestionnaires
    , formValidation = EditableQuestionnairesConfigForm.validation appState
    }
