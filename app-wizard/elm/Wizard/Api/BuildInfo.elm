module Wizard.Api.BuildInfo exposing (getBuildInfo)

import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.BuildInfo as BuildInfo exposing (BuildInfo)
import Wizard.Data.AppState as AppState exposing (AppState)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Request.get (AppState.toServerInfo appState) "" BuildInfo.decoder
