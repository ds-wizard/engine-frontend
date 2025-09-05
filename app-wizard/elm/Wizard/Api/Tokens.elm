module Wizard.Api.Tokens exposing
    ( deleteCurrentToken
    , deleteToken
    , deleteTokens
    , fetchToken
    , getTokens
    )

import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Wizard.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Wizard.Data.AppState as AppState exposing (AppState)


fetchToken : AppState -> E.Value -> ToMsg TokenResponse msg -> Cmd msg
fetchToken appState body =
    Request.post (AppState.toServerInfo appState) "/tokens" TokenResponse.decoder body


getTokens : AppState -> ToMsg (List ApiKey) msg -> Cmd msg
getTokens appState =
    Request.get (AppState.toServerInfo appState) "/tokens" (D.list ApiKey.decoder)


deleteToken : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteToken appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/tokens/" ++ Uuid.toString uuid)


deleteCurrentToken : AppState -> ToMsg () msg -> Cmd msg
deleteCurrentToken appState =
    Request.delete (AppState.toServerInfo appState) "/tokens/current"


deleteTokens : AppState -> ToMsg () msg -> Cmd msg
deleteTokens appState =
    Request.delete (AppState.toServerInfo appState) "/tokens"
