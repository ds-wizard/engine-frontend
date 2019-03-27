module Common.Api.BuildInfo exposing (getBuildInfo)

import Common.Api exposing (ToMsg, httpGet)
import Common.AppState exposing (AppState)
import Common.Menu.Models exposing (BuildInfo, buildInfoDecoder)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo =
    httpGet "" buildInfoDecoder
