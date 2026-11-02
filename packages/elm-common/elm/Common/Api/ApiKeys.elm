module Common.Api.ApiKeys exposing
    ( deleteApiKey
    , fetchApiKey
    , getApiKeys
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


getApiKeys : Context ctx -> ToMsg (List ApiKey) msg -> Cmd msg
getApiKeys appState =
    Request.get (Context.toServerInfo appState) "/api-keys" (D.list ApiKey.decoder)


fetchApiKey : Context ctx -> E.Value -> ToMsg String msg -> Cmd msg
fetchApiKey appState body =
    Request.post (Context.toServerInfo appState) "/api-keys" (D.field "token" D.string) body


deleteApiKey : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteApiKey appState uuid =
    Request.delete (Context.toServerInfo appState) ("/api-keys/" ++ Uuid.toString uuid)
