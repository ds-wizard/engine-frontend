module Wizard.Api.ActionKeys exposing (postActionKey)

import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E
import Wizard.Data.AppState as AppState exposing (AppState)


postActionKey : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postActionKey appState =
    Request.postWhatever (AppState.toServerInfo appState) "/action-keys"
