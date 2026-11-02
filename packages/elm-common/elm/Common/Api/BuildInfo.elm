module Common.Api.BuildInfo exposing (getBuildInfo)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.BuildInfo as BuildInfo exposing (BuildInfo)
import Common.Api.Request as Request exposing (ToMsg)


getBuildInfo : Context ctx -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Request.get (Context.toServerInfo appState) "" BuildInfo.decoder
