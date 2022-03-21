module Shared.Api.Prefabs exposing (getIntegrationPrefabs, getOpenIDPrefabs)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig as EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.Prefab as Prefab exposing (Prefab)


getIntegrationPrefabs : AbstractAppState a -> ToMsg (List (Prefab Integration)) msg -> Cmd msg
getIntegrationPrefabs =
    jwtGet "/prefabs?type=knowledge-model-integration" (D.list (Prefab.decoder Integration.decoder))


getOpenIDPrefabs : AbstractAppState a -> ToMsg (List (Prefab EditableOpenIDServiceConfig)) msg -> Cmd msg
getOpenIDPrefabs =
    jwtGet "/prefabs?type=open-id" (D.list (Prefab.decoder EditableOpenIDServiceConfig.decoder))
