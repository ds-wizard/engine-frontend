module KnowledgeModels.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Encode exposing (Value)
import KnowledgeModels.Models exposing (KnowledgeModel, knowledgeModelListDecoder)
import Requests


getKnowledgeModels : Session -> Http.Request (List KnowledgeModel)
getKnowledgeModels session =
    Requests.get session "/kmcs" knowledgeModelListDecoder


postKnowledgeModel : Session -> Value -> Http.Request String
postKnowledgeModel session knowledgeModel =
    Requests.post knowledgeModel session "/kmcs"


deleteKnowledgeModel : String -> Session -> Http.Request String
deleteKnowledgeModel kmId session =
    Requests.delete session ("/kmcs/" ++ kmId)
