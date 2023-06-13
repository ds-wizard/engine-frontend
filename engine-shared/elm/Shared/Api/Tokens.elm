module Shared.Api.Tokens exposing
    ( deleteCurrentToken
    , deleteToken
    , fetchToken
    , getTokens
    )

import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpFetch, jwtDelete, jwtGet)
import Shared.Data.ApiKey as ApiKey exposing (ApiKey)
import Shared.Data.TokenResponse as TokenResponse exposing (TokenResponse)
import Uuid exposing (Uuid)


fetchToken : E.Value -> AbstractAppState a -> ToMsg TokenResponse msg -> Cmd msg
fetchToken =
    httpFetch "/tokens" TokenResponse.decoder


getTokens : AbstractAppState a -> ToMsg (List ApiKey) msg -> Cmd msg
getTokens =
    jwtGet "/tokens" (D.list ApiKey.decoder)


deleteToken : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteToken uuid =
    jwtDelete ("/tokens/" ++ Uuid.toString uuid)


deleteCurrentToken : AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteCurrentToken =
    jwtDelete "/tokens/current"
