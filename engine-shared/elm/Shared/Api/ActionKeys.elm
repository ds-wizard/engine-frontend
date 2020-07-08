module Shared.Api.ActionKeys exposing (postActionKey)

import Json.Encode exposing (Value)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpPost)


postActionKey : Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postActionKey =
    httpPost "/action-keys"
