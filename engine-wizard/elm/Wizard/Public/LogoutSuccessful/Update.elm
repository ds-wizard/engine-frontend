module Wizard.Public.LogoutSuccessful.Update exposing (fetchData)

import Shared.Auth.Session as Session
import Wizard.Common.AppState exposing (AppState)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd msg
fetchData appState =
    if Session.exists appState.session then
        cmdNavigate appState Routes.appHome

    else
        Cmd.none
