module Registry.Api.BuildInfo exposing (getBuildInfo)

import Registry.Data.AppState as AppState exposing (AppState)
import Shared.Api.Request as Requests exposing (ToMsg)
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Requests.get (AppState.toServerInfo appState) "" BuildInfo.decoder
