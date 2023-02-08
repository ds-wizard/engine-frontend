module Shared.Api.Tokens exposing (fetchToken)

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpFetch)
import Shared.Data.TokenResponse as TokenResponse exposing (TokenResponse)


fetchToken : E.Value -> AbstractAppState a -> ToMsg TokenResponse msg -> Cmd msg
fetchToken =
    httpFetch "/tokens" TokenResponse.decoder
