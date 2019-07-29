module KMEditor.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Branch exposing (Branch)
import KnowledgeModels.Common.Package exposing (Package)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | PostBranchCompleted (Result ApiError Branch)
