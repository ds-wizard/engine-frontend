module Common.Api.Levels exposing (getLevels)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import KMEditor.Common.Models.Entities exposing (Level, Metric, levelListDecoder)


getLevels : AppState -> ToMsg (List Level) msg -> Cmd msg
getLevels =
    jwtGet "/levels" levelListDecoder
