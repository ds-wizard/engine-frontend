module Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
