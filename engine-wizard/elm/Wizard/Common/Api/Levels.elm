module Wizard.Common.Api.Levels exposing (getLevels)

import Json.Decode as D
import Wizard.Common.Api exposing (ToMsg, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Common.KnowledgeModel.Level as Level exposing (Level)


getLevels : AppState -> ToMsg (List Level) msg -> Cmd msg
getLevels =
    jwtGet "/levels" (D.list Level.decoder)
