module Common.Api.Prefabs exposing
    ( getDocumentTemplateFormatPrefabs
    , getDocumentTemplateFormatStepPrefabs
    , getIntegrationPrefabs
    , getOpenIDPrefabs
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFormatStep as DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Common.Api.Models.OpenIdClientDetail as OpenIdClientDetail exposing (OpenIdClientDetail)
import Common.Api.Models.Prefab as Prefab exposing (Prefab)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D


getIntegrationPrefabs : Context ctx -> ToMsg (List (Prefab Integration)) msg -> Cmd msg
getIntegrationPrefabs appState =
    Request.get (Context.toServerInfo appState) "/prefabs?type=knowledge-model-integration" (D.list (Prefab.decoder Integration.decoder))


getOpenIDPrefabs : Context ctx -> ToMsg (List (Prefab OpenIdClientDetail)) msg -> Cmd msg
getOpenIDPrefabs appState =
    Request.get (Context.toServerInfo appState) "/prefabs?type=open-id" (D.list (Prefab.decoder OpenIdClientDetail.prefabDecoder))


getDocumentTemplateFormatPrefabs : Context ctx -> ToMsg (List (Prefab DocumentTemplateFormatDraft)) msg -> Cmd msg
getDocumentTemplateFormatPrefabs appState =
    Request.get (Context.toServerInfo appState) "/prefabs?type=document-template-format" (D.list (Prefab.decoder DocumentTemplateFormatDraft.decoder))


getDocumentTemplateFormatStepPrefabs : Context ctx -> ToMsg (List (Prefab DocumentTemplateFormatStep)) msg -> Cmd msg
getDocumentTemplateFormatStepPrefabs appState =
    Request.get (Context.toServerInfo appState) "/prefabs?type=document-template-format-step" (D.list (Prefab.decoder DocumentTemplateFormatStep.decoder))
