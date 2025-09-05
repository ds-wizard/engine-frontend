module Wizard.Api.Prefabs exposing
    ( getDocumentTemplateFormatPrefabs
    , getDocumentTemplateFormatStepPrefabs
    , getIntegrationPrefabs
    , getOpenIDPrefabs
    )

import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.Prefab as Prefab exposing (Prefab)
import Json.Decode as D
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep as DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig as EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Data.AppState as AppState exposing (AppState)


getIntegrationPrefabs : AppState -> ToMsg (List (Prefab Integration)) msg -> Cmd msg
getIntegrationPrefabs appState =
    Request.get (AppState.toServerInfo appState) "/prefabs?type=knowledge-model-integration" (D.list (Prefab.decoder Integration.decoder))


getOpenIDPrefabs : AppState -> ToMsg (List (Prefab EditableOpenIDServiceConfig)) msg -> Cmd msg
getOpenIDPrefabs appState =
    Request.get (AppState.toServerInfo appState) "/prefabs?type=open-id" (D.list (Prefab.decoder EditableOpenIDServiceConfig.decoder))


getDocumentTemplateFormatPrefabs : AppState -> ToMsg (List (Prefab DocumentTemplateFormatDraft)) msg -> Cmd msg
getDocumentTemplateFormatPrefabs appState =
    Request.get (AppState.toServerInfo appState) "/prefabs?type=document-template-format" (D.list (Prefab.decoder DocumentTemplateFormatDraft.decoder))


getDocumentTemplateFormatStepPrefabs : AppState -> ToMsg (List (Prefab DocumentTemplateFormatStep)) msg -> Cmd msg
getDocumentTemplateFormatStepPrefabs appState =
    Request.get (AppState.toServerInfo appState) "/prefabs?type=document-template-format-step" (D.list (Prefab.decoder DocumentTemplateFormatStep.decoder))
