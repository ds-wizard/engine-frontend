module Shared.Api.Usage exposing (getUsage)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.Usage as Usage exposing (Usage)


getUsage : AbstractAppState a -> ToMsg Usage msg -> Cmd msg
getUsage =
    jwtGet "/usage" Usage.decoder
