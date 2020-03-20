module Wizard.Settings.Organization.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.EditableOrganizationConfig as EditableOrganizationConfig exposing (EditableOrganizationConfig)
import Wizard.Settings.Common.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Organization.Models exposing (Model)
import Wizard.Settings.Organization.Msgs exposing (Msg)


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableOrganizationConfig OrganizationConfigForm
updateProps =
    { initForm = OrganizationConfigForm.init
    , getConfig = ConfigsApi.getOrganizationConfig
    , putConfig = ConfigsApi.putOrganizationConfig
    , locApiGetError = lg "apiError.config.organization.getError"
    , locApiPutError = lg "apiError.config.organization.putError"
    , encodeConfig = EditableOrganizationConfig.encode
    , formToConfig = OrganizationConfigForm.toEditableOrganizationConfig
    , formValidation = OrganizationConfigForm.validation
    }
