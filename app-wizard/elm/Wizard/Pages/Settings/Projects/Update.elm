module Wizard.Pages.Settings.Projects.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate
import Wizard.Pages.Settings.Projects.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState =
    GenericUpdate.update (updateProps appState) wrapMsg msg appState


updateProps : AppState -> GenericUpdate.UpdateProps EditableQuestionnairesConfigForm
updateProps appState =
    { initForm = .questionnaires >> EditableQuestionnairesConfigForm.init appState
    , formToConfig = EditableQuestionnairesConfigForm.toEditableQuestionnaireConfig >> EditableConfig.updateQuestionnaires
    , formValidation = EditableQuestionnairesConfigForm.validation appState
    }
