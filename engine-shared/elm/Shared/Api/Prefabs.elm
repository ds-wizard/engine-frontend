module Shared.Api.Prefabs exposing (getIntegrationPrefabs)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.Prefab as Prefab exposing (Prefab)


getIntegrationPrefabs : AbstractAppState a -> ToMsg (List (Prefab Integration)) msg -> Cmd msg
getIntegrationPrefabs =
    jwtGet "/prefabs?type=knowledge-model-integration" (D.list (Prefab.decoder Integration.decoder))
