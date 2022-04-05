module Wizard.Settings.Plans.Update exposing (fetchData, update)

import Shared.Api.Apps as AppsApi
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setPlans)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Plans.Models exposing (Model)
import Wizard.Settings.Plans.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    AppsApi.getCurrentPlans appState GetPlansComplete


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetPlansComplete result ->
            applyResult appState
                { setResult = setPlans
                , defaultError = lg "apiError.usage.getError" appState
                , model = model
                , result = result
                }
