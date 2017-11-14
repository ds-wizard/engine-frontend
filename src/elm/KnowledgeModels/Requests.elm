module KnowledgeModels.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Encode exposing (Value)
import KnowledgeModels.Models exposing (KnowledgeModel, knowledgeModelDecoder, knowledgeModelListDecoder)
import Requests


getKnowledgeModel : String -> Session -> Http.Request KnowledgeModel
getKnowledgeModel uuid session =
    Requests.get session ("/kmcs/" ++ uuid) knowledgeModelDecoder


getKnowledgeModels : Session -> Http.Request (List KnowledgeModel)
getKnowledgeModels session =
    Requests.get session "/kmcs" knowledgeModelListDecoder


postKnowledgeModel : Session -> Value -> Http.Request String
postKnowledgeModel session knowledgeModel =
    Requests.post knowledgeModel session "/kmcs"


deleteKnowledgeModel : String -> Session -> Http.Request String
deleteKnowledgeModel uuid session =
    Requests.delete session ("/kmcs/" ++ uuid)


putKnowledgeModelVersion : String -> String -> Value -> Session -> Http.Request String
putKnowledgeModelVersion kmUuid version data session =
    Requests.put data session ("/kmcs/" ++ kmUuid ++ "/versions/" ++ version)
