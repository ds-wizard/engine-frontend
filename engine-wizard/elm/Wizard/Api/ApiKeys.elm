module Wizard.Api.ApiKeys exposing
    ( deleteApiKey
    , fetchApiKey
    , getApiKeys
    )

import Json.Decode as D
import Json.Encode as E
import Shared.Api.Request as Request exposing (ToMsg)
import Uuid exposing (Uuid)
import Wizard.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Wizard.Common.AppState as AppState exposing (AppState)


getApiKeys : AppState -> ToMsg (List ApiKey) msg -> Cmd msg
getApiKeys appState =
    Request.get (AppState.toServerInfo appState) "/api-keys" (D.list ApiKey.decoder)


fetchApiKey : AppState -> E.Value -> ToMsg String msg -> Cmd msg
fetchApiKey appState body =
    Request.post (AppState.toServerInfo appState) "/api-keys" (D.field "token" D.string) body


deleteApiKey : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteApiKey appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/api-keys/" ++ Uuid.toString uuid)
