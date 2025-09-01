module Wizard.Api.ActionKeys exposing (postActionKey)

import Json.Encode as E
import Shared.Api.Request as Request exposing (ToMsg)
import Wizard.Common.AppState as AppState exposing (AppState)


postActionKey : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postActionKey appState =
    Request.postWhatever (AppState.toServerInfo appState) "/action-keys"
