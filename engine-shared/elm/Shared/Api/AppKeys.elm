module Shared.Api.AppKeys exposing (deleteAppKey, getAppKeys)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet)
import Shared.Data.AppKey as AppKey exposing (AppKey)
import Uuid exposing (Uuid)


getAppKeys : AbstractAppState a -> ToMsg (List AppKey) msg -> Cmd msg
getAppKeys =
    jwtGet "/app-keys" (D.list AppKey.decoder)


deleteAppKey : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteAppKey uuid =
    jwtDelete ("/app-keys/" ++ Uuid.toString uuid)
