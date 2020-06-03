module Shared.Api.Tokens exposing (fetchToken)

import Json.Encode as E
import Shared.Api exposing (AppStateLike, ToMsg, httpFetch)
import Shared.Data.Token as Token exposing (Token)


fetchToken : E.Value -> AppStateLike a -> ToMsg Token msg -> Cmd msg
fetchToken =
    httpFetch "/tokens" Token.decoder
