module Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
