module Shared.Api.BuildInfo exposing (getBuildInfo)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpGet)
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)


getBuildInfo : AbstractAppState a -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo =
    httpGet "" BuildInfo.decoder
