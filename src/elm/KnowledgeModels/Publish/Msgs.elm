module KnowledgeModels.Publish.Msgs exposing (..)

import Form
import Jwt
import KnowledgeModels.Models exposing (KnowledgeModel)


type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | FormMsg Form.Msg
    | PutKnowledgeModelVersionCompleted (Result Jwt.JwtError String)
