module Wizard.Settings.Features.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.FeaturesConfigForm as FeaturesConfigForm exposing (FeaturesConfigForm)
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Features.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate


fetchData : AppState -> Cmd Msg
fetchData =
    GenericUpdate.fetchData updateProps


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableFeaturesConfig FeaturesConfigForm
updateProps =
    { initForm = FeaturesConfigForm.init
    , getConfig = ConfigsApi.getFeaturesConfig
    , putConfig = ConfigsApi.putFeaturesConfig
    , locApiGetError = lg "apiError.config.features.getError"
    , locApiPutError = lg "apiError.config.features.putError"
    , encodeConfig = EditableFeaturesConfig.encode
    , formToConfig = FeaturesConfigForm.toEditableFeaturesConfig
    , formValidation = FeaturesConfigForm.validation
    }
