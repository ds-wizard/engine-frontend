module KMEditor.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | PostKnowledgeModelCompleted (Result ApiError KnowledgeModel)
