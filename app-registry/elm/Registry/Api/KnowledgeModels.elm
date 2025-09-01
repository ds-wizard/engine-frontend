module Registry.Api.KnowledgeModels exposing (getKnowledgeModel, getKnowledgeModels)

import Json.Decode as D
import Registry.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Registry.Api.Models.KnowledgeModelDetail as KnowledgeModelDetail exposing (KnowledgeModelDetail)
import Registry.Data.AppState as AppState exposing (AppState)
import Shared.Api.Request as Requests exposing (ToMsg)


getKnowledgeModels : AppState -> ToMsg (List KnowledgeModel) msg -> Cmd msg
getKnowledgeModels appState =
    Requests.get (AppState.toServerInfo appState) "/packages" (D.list KnowledgeModel.decoder)


getKnowledgeModel : AppState -> String -> ToMsg KnowledgeModelDetail msg -> Cmd msg
getKnowledgeModel appState packageId =
    Requests.get (AppState.toServerInfo appState) ("/packages/" ++ packageId) KnowledgeModelDetail.decoder
