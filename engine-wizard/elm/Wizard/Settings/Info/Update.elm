module Wizard.Settings.Info.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.EditableInfoConfig as EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.InfoConfigForm as InfoConfigForm exposing (InfoConfigForm)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Info.Models exposing (Model)
import Wizard.Settings.Info.Msgs exposing (Msg)


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableInfoConfig InfoConfigForm
updateProps =
    { initForm = InfoConfigForm.init
    , getConfig = ConfigsApi.getInfoConfig
    , putConfig = ConfigsApi.putInfoConfig
    , locApiGetError = lg "apiError.config.info.getError"
    , locApiPutError = lg "apiError.config.info.putError"
    , encodeConfig = EditableInfoConfig.encode
    , formToConfig = InfoConfigForm.toEditableInfoConfig
    , formValidation = InfoConfigForm.validation
    }
