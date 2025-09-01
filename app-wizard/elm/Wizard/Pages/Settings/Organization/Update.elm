module Wizard.Pages.Settings.Organization.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Common.Forms.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate
import Wizard.Pages.Settings.Organization.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState =
    GenericUpdate.update (updateProps appState) wrapMsg msg appState


updateProps : AppState -> GenericUpdate.UpdateProps OrganizationConfigForm
updateProps appState =
    { initForm = .organization >> OrganizationConfigForm.init appState
    , formToConfig = OrganizationConfigForm.toOrganizationConfig >> EditableConfig.updateOrganization
    , formValidation = OrganizationConfigForm.validation appState
    }
