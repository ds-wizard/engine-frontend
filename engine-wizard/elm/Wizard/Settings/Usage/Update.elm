module Wizard.Settings.Usage.Update exposing (fetchData, update)

import Gettext exposing (gettext)
import Shared.Api.Usage as UsageApi
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
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }
