module Wizard.Settings.Organization.Update exposing (update)

import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Organization.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps OrganizationConfigForm
updateProps =
    { initForm = .organization >> OrganizationConfigForm.init
    , formToConfig = OrganizationConfigForm.toOrganizationConfig >> EditableConfig.updateOrganization
    , formValidation = OrganizationConfigForm.validation
    }
