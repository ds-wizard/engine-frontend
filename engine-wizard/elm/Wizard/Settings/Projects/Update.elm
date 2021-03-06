module Wizard.Settings.Projects.Update exposing (update)

import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm as EditableQuestionnairesConfigForm exposing (EditableQuestionnairesConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Projects.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableQuestionnairesConfigForm
updateProps =
    { initForm = .questionnaires >> EditableQuestionnairesConfigForm.init
    , formToConfig = EditableQuestionnairesConfigForm.toEditableQuestionnaireConfig >> EditableConfig.updateQuestionnaires
    , formValidation = EditableQuestionnairesConfigForm.validation
    }
