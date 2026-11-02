module Common.Api.KnowledgeModelSecrets exposing (deleteKnowledgeModelSecret, getKnowledgeModelSecrets, postKnowledgeModelSecret, putKnowledgeModelSecret)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.KnowledgeModelSecret as KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.Forms.KnowledgeModelSecretForm as KnowledgeModelSecretForm exposing (KnowledgeModelSecretForm)
import Json.Decode as D
import Uuid exposing (Uuid)


getKnowledgeModelSecrets : Context ctx -> ToMsg (List KnowledgeModelSecret) msg -> Cmd msg
getKnowledgeModelSecrets appState =
    Request.get (Context.toServerInfo appState) (Params.apply appState "GET" "/knowledge-model-secrets") (D.list KnowledgeModelSecret.decoder)


postKnowledgeModelSecret : Context ctx -> KnowledgeModelSecretForm -> ToMsg () msg -> Cmd msg
postKnowledgeModelSecret appState form =
    let
        data =
            KnowledgeModelSecretForm.encode form
    in
    Request.postWhatever (Context.toServerInfo appState) (Params.apply appState "POST" "/knowledge-model-secrets") data


putKnowledgeModelSecret : Context ctx -> Uuid -> KnowledgeModelSecretForm -> ToMsg () msg -> Cmd msg
putKnowledgeModelSecret appState secretId form =
    let
        data =
            KnowledgeModelSecretForm.encode form
    in
    Request.putWhatever (Context.toServerInfo appState) ("/knowledge-model-secrets/" ++ Uuid.toString secretId) data


deleteKnowledgeModelSecret : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelSecret appState secretId =
    Request.delete (Context.toServerInfo appState) ("/knowledge-model-secrets/" ++ Uuid.toString secretId)
