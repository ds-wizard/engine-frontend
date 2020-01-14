module Wizard.Common.Api.BuildInfo exposing (getBuildInfo)

import Wizard.Common.Api exposing (ToMsg, httpGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Menu.Models exposing (BuildInfo, buildInfoDecoder)


getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo =
    httpGet "" buildInfoDecoder
