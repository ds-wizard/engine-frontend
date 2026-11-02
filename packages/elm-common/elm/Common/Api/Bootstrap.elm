module Common.Api.Bootstrap exposing (getWorkspace)

{-| The second part of what the application is started with
(`Common.Api.Models.BootstrapConfig`), for somebody signed in. Where it is
read for is the application's to say (`Common.Api.Params`).
-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (WorkspaceConfig)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)


getWorkspace : Context ctx -> ToMsg WorkspaceConfig msg -> Cmd msg
getWorkspace appState =
    Request.get (Context.toServerInfo appState)
        (Params.apply appState "GET" "/bootstrap/workspace")
        BootstrapConfig.workspaceDecoder
