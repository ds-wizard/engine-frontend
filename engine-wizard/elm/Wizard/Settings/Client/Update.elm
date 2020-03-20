module Wizard.Settings.Client.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Client.Models exposing (Model)
import Wizard.Settings.Client.Msgs exposing (Msg)
import Wizard.Settings.Common.ClientConfigForm as ClientConfigForm exposing (ClientConfigForm)
import Wizard.Settings.Common.EditableClientConfig as EditableClientConfig exposing (EditableClientConfig)
import Wizard.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableClientConfig ClientConfigForm
updateProps =
    { initForm = ClientConfigForm.init
    , getConfig = ConfigsApi.getClientConfig
    , putConfig = ConfigsApi.putClientConfig
    , locApiGetError = lg "apiError.config.client.getError"
    , locApiPutError = lg "apiError.config.client.putError"
    , encodeConfig = EditableClientConfig.encode
    , formToConfig = ClientConfigForm.toEditableClientConfig
    , formValidation = ClientConfigForm.validation
    }
