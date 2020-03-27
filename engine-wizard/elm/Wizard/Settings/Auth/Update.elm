module Wizard.Settings.Auth.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Auth.Models exposing (Model)
import Wizard.Settings.Auth.Msgs exposing (Msg)
import Wizard.Settings.Common.AuthConfigForm as AuthConfigForm exposing (AuthConfigForm)
import Wizard.Settings.Common.EditableAuthConfig as EditableAuthConfig exposing (EditableAuthConfig)
import Wizard.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableAuthConfig AuthConfigForm
updateProps =
    { initForm = AuthConfigForm.init
    , getConfig = ConfigsApi.getAuthConfig
    , putConfig = ConfigsApi.putAuthConfig
    , locApiGetError = lg "apiError.config.client.getError"
    , locApiPutError = lg "apiError.config.client.putError"
    , encodeConfig = EditableAuthConfig.encode
    , formToConfig = AuthConfigForm.toEditableAuthConfig
    , formValidation = AuthConfigForm.validation
    }
