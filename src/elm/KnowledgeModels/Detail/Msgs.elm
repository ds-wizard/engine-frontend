module KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
