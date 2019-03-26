module KMEditor.Publish.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models exposing (KnowledgeModelDetail)


type Msg
    = GetKnowledgeModelCompleted (Result ApiError KnowledgeModelDetail)
    | FormMsg Form.Msg
    | PutKnowledgeModelVersionCompleted (Result ApiError ())
