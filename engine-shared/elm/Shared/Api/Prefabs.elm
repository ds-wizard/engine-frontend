module Shared.Api.Prefabs exposing (getDocumentTemplateFormatPrefabs, getDocumentTemplateFormatStepPrefabs, getIntegrationPrefabs, getOpenIDPrefabs)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.DocumentTemplate.DocumentTemplateFormatStep as DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Shared.Data.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig as EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.Prefab as Prefab exposing (Prefab)


getIntegrationPrefabs : AbstractAppState a -> ToMsg (List (Prefab Integration)) msg -> Cmd msg
getIntegrationPrefabs =
    jwtGet "/prefabs?type=knowledge-model-integration" (D.list (Prefab.decoder Integration.decoder))


getOpenIDPrefabs : AbstractAppState a -> ToMsg (List (Prefab EditableOpenIDServiceConfig)) msg -> Cmd msg
getOpenIDPrefabs =
    jwtGet "/prefabs?type=open-id" (D.list (Prefab.decoder EditableOpenIDServiceConfig.decoder))


getDocumentTemplateFormatPrefabs : AbstractAppState a -> ToMsg (List (Prefab DocumentTemplateFormatDraft)) msg -> Cmd msg
getDocumentTemplateFormatPrefabs =
    jwtGet "/prefabs?type=document-template-format" (D.list (Prefab.decoder DocumentTemplateFormatDraft.decoder))


getDocumentTemplateFormatStepPrefabs : AbstractAppState a -> ToMsg (List (Prefab DocumentTemplateFormatStep)) msg -> Cmd msg
getDocumentTemplateFormatStepPrefabs =
    jwtGet "/prefabs?type=document-template-format-step" (D.list (Prefab.decoder DocumentTemplateFormatStep.decoder))
