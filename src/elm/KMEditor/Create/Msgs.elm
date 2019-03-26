module KMEditor.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List PackageDetail))
    | PostKnowledgeModelCompleted (Result ApiError KnowledgeModel)
