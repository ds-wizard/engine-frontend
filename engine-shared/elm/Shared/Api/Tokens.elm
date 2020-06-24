module Shared.Api.Tokens exposing (fetchToken)

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpFetch)
import Shared.Data.Token as Token exposing (Token)


fetchToken : E.Value -> AbstractAppState a -> ToMsg Token msg -> Cmd msg
fetchToken =
    httpFetch "/tokens" Token.decoder
