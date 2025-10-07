module Registry.Api.BuildInfo exposing (getBuildInfo)

import Common.Api.Models.BuildInfo as BuildInfo exposing (BuildInfo)
import Common.Api.Request as Requests exposing (ToMsg)
import Registry.Data.AppState as AppState exposing (AppState)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Requests.get (AppState.toServerInfo appState) "" BuildInfo.decoder
