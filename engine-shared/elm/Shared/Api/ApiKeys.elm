module Shared.Api.ApiKeys exposing (deleteApiKey, fetchApiKey, getApiKeys)

import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet)
import Shared.Data.ApiKey as ApiKey exposing (ApiKey)
import Uuid exposing (Uuid)


getApiKeys : AbstractAppState a -> ToMsg (List ApiKey) msg -> Cmd msg
getApiKeys =
    jwtGet "/api-keys" (D.list ApiKey.decoder)


fetchApiKey : E.Value -> AbstractAppState a -> ToMsg String msg -> Cmd msg
fetchApiKey body =
    jwtFetch "/api-keys" (D.field "token" D.string) body


deleteApiKey : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteApiKey uuid =
    jwtDelete ("/api-keys/" ++ Uuid.toString uuid)
