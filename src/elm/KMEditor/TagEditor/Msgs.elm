module KMEditor.TagEditor.Msgs exposing (Msg(..))

import Jwt
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)


type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | Highlight String
    | CancelHighlight
    | AddTag String String
    | RemoveTag String String
    | Submit
    | SubmitCompleted (Result Jwt.JwtError String)
    | Discard
