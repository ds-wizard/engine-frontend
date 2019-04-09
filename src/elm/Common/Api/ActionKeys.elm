module Common.Api.ActionKeys exposing (postActionKey)

import Common.Api exposing (ToMsg, httpPost)
import Common.AppState exposing (AppState)
import Json.Encode exposing (Value)


postActionKey : Value -> AppState -> ToMsg () msg -> Cmd msg
postActionKey =
    httpPost "/action-keys"
