module Registry.Api.BuildInfo exposing (getBuildInfo)

import Registry.Api.Requests as Requests
import Registry.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Requests.get appState "" BuildInfo.decoder
