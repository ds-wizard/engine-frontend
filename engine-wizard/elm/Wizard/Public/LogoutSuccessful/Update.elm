module Wizard.Public.LogoutSuccessful.Update exposing (fetchData)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd msg
fetchData appState =
    if Session.exists appState.session then
        cmdNavigate appState Routes.appHome

    else
        Cmd.none
