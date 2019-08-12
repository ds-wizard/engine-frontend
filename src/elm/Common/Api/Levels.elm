module Common.Api.Levels exposing (getLevels)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import Json.Decode as D
import KMEditor.Common.KnowledgeModel.Level as Level exposing (Level)


getLevels : AppState -> ToMsg (List Level) msg -> Cmd msg
getLevels =
    jwtGet "/levels" (D.list Level.decoder)
