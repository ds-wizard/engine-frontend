module Wizard.Api.AppKeys exposing
    ( deleteAppKey
    , getAppKeys
    )

import Json.Decode as D
import Shared.Api.Request as Request exposing (ToMsg)
import Uuid exposing (Uuid)
import Wizard.Api.Models.AppKey as AppKey exposing (AppKey)
import Wizard.Data.AppState as AppState exposing (AppState)


getAppKeys : AppState -> ToMsg (List AppKey) msg -> Cmd msg
getAppKeys appState =
    Request.get (AppState.toServerInfo appState) "/app-keys" (D.list AppKey.decoder)


deleteAppKey : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteAppKey appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/app-keys/" ++ Uuid.toString uuid)
