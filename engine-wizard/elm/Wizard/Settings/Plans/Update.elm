module Wizard.Settings.Plans.Update exposing (fetchData, update)

import Gettext exposing (gettext)
import Shared.Api.Tenants as TenantsApi
import Shared.Setters exposing (setPlans)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Plans.Models exposing (Model)
import Wizard.Settings.Plans.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    TenantsApi.getCurrentPlans appState GetPlansComplete


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetPlansComplete result ->
            applyResult appState
                { setResult = setPlans
                , defaultError = gettext "Unable to get plans." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }
