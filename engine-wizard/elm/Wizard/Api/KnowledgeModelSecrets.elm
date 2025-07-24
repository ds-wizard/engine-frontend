module Wizard.Api.KnowledgeModelSecrets exposing (deleteKnowledgeModelSecret, getKnowledgeModelSecrets, postKnowledgeModelSecret, putKnowledgeModelSecret)

import Json.Decode as D
import Shared.Api.Request as Request exposing (ToMsg)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelSecret as KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.KnowledgeModelSecrets.Forms.KnowledgeModelSecretForm as KnowledgeModelSecretForm exposing (KnowledgeModelSecretForm)


getKnowledgeModelSecrets : AppState -> ToMsg (List KnowledgeModelSecret) msg -> Cmd msg
getKnowledgeModelSecrets appState =
    Request.get (AppState.toServerInfo appState) "/knowledge-model-secrets" (D.list KnowledgeModelSecret.decoder)


postKnowledgeModelSecret : AppState -> KnowledgeModelSecretForm -> ToMsg () msg -> Cmd msg
postKnowledgeModelSecret appState form =
    let
        data =
            KnowledgeModelSecretForm.encode form
    in
    Request.postWhatever (AppState.toServerInfo appState) "/knowledge-model-secrets" data


putKnowledgeModelSecret : AppState -> Uuid -> KnowledgeModelSecretForm -> ToMsg () msg -> Cmd msg
putKnowledgeModelSecret appState secretId form =
    let
        data =
            KnowledgeModelSecretForm.encode form
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/knowledge-model-secrets/" ++ Uuid.toString secretId) data


deleteKnowledgeModelSecret : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelSecret appState secretId =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-secrets/" ++ Uuid.toString secretId)
