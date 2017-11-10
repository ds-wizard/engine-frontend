module KnowledgeModels.Index.Msgs exposing (..)

import Jwt
import KnowledgeModels.Models exposing (KnowledgeModel)


type Msg
    = GetKnowledgeModelsCompleted (Result Jwt.JwtError (List KnowledgeModel))
