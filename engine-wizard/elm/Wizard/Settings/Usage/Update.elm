module Wizard.Settings.Usage.Update exposing (fetchData, update)

import Shared.Api.Usage as UsageApi
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setUsage)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Usage.Models exposing (Model)
import Wizard.Settings.Usage.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsageApi.getUsage appState GetUsageComplete


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetUsageComplete result ->
            applyResult appState
                { setResult = setUsage
                , defaultError = lg "apiError.usage.getError" appState
                , model = model
                , result = result
                }
