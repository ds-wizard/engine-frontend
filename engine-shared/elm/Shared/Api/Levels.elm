module Shared.Api.Levels exposing (getLevels)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.KnowledgeModel.Level as Level exposing (Level)


getLevels : AbstractAppState a -> ToMsg (List Level) msg -> Cmd msg
getLevels =
    jwtGet "/levels" (D.list Level.decoder)
