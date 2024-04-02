module Registry.Api.KnowledgeModels exposing (getKnowledgeModel, getKnowledgeModels)

import Json.Decode as D
import Registry.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Registry.Api.Models.KnowledgeModelDetail as KnowledgeModelDetail exposing (KnowledgeModelDetail)
import Registry.Api.Requests as Requests
import Registry.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)


getKnowledgeModels : AppState -> ToMsg (List KnowledgeModel) msg -> Cmd msg
getKnowledgeModels appState =
    Requests.get appState "/packages" (D.list KnowledgeModel.decoder)


getKnowledgeModel : AppState -> String -> ToMsg KnowledgeModelDetail msg -> Cmd msg
getKnowledgeModel appState packageId =
    Requests.get appState ("/packages/" ++ packageId) KnowledgeModelDetail.decoder
