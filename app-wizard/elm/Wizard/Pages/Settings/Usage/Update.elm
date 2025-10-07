module Wizard.Pages.Settings.Usage.Update exposing (fetchData, update)

import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setUsage)
import Gettext exposing (gettext)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Usage.Models exposing (Model)
import Wizard.Pages.Settings.Usage.Msgs exposing (Msg(..))


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
