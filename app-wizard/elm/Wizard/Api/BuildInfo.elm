module Wizard.Api.BuildInfo exposing (getBuildInfo)

import Common.Api.Models.BuildInfo as BuildInfo exposing (BuildInfo)
import Common.Api.Request as Request exposing (ToMsg)
import Wizard.Data.AppState as AppState exposing (AppState)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Request.get (AppState.toServerInfo appState) "" BuildInfo.decoder
