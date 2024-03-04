module Registry2.Api.BuildInfo exposing (getBuildInfo)

import Registry2.Api.Requests as Requests
import Registry2.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Requests.get appState "" BuildInfo.decoder
