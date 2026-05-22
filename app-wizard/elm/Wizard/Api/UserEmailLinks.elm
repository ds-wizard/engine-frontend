module Wizard.Api.UserEmailLinks exposing (postUserEmailLink)

import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E
import Wizard.Data.AppState as AppState exposing (AppState)


postUserEmailLink : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postUserEmailLink appState =
    Request.postWhatever (AppState.toServerInfo appState) "/user-email-links"
