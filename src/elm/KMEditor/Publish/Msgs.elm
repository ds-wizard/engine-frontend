module KMEditor.Publish.Msgs exposing (..)

{-|

@docs Msg

-}

import Form
import Jwt
import KMEditor.Models exposing (KnowledgeModel)


{-| -}
type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | FormMsg Form.Msg
    | PutKnowledgeModelVersionCompleted (Result Jwt.JwtError String)
