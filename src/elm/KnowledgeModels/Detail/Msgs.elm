module KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.ApiError exposing (ApiError)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result ApiError (List PackageDetail))
    | ShowHideDeleteVersion (Maybe String)
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | DropdownMsg PackageDetail Dropdown.State
