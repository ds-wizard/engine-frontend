module Wizard.Common.Api.ActionKeys exposing (postActionKey)

import Json.Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, httpPost)
import Wizard.Common.AppState exposing (AppState)


postActionKey : Value -> AppState -> ToMsg () msg -> Cmd msg
postActionKey =
    httpPost "/action-keys"
