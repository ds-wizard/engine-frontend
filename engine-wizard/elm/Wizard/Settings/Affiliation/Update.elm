module Wizard.Settings.Affiliation.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Affiliation.Models exposing (Model)
import Wizard.Settings.Affiliation.Msgs exposing (Msg)
import Wizard.Settings.Common.AffiliationConfigForm as AffiliationConfigForm exposing (AffiliationConfigForm)
import Wizard.Settings.Common.EditableAffiliationConfig as EditableAffiliationConfig exposing (EditableAffiliationConfig)
import Wizard.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableAffiliationConfig AffiliationConfigForm
updateProps =
    { initForm = AffiliationConfigForm.init
    , getConfig = ConfigsApi.getAffiliationConfig
    , putConfig = ConfigsApi.putAffiliationConfig
    , locApiGetError = lg "apiError.config.affiliation.getError"
    , locApiPutError = lg "apiError.config.affiliation.putError"
    , encodeConfig = EditableAffiliationConfig.encode
    , formToConfig = AffiliationConfigForm.toEditableAffiliationConfig
    , formValidation = AffiliationConfigForm.validation
    }
