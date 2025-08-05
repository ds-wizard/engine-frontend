module Wizard.Settings.Usage.Update exposing (fetchData, update)

import Gettext exposing (gettext)
import Shared.Common.UuidOrCurrent as UuidOrCurrent
import Shared.Setters exposing (setUsage)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Tenants as TenantsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Usage.Models exposing (Model)
import Wizard.Settings.Usage.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    TenantsApi.getTenantUsage appState UuidOrCurrent.current GetUsageComplete


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetUsageComplete result ->
            RequestHelpers.applyResult
                { setResult = setUsage
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }
