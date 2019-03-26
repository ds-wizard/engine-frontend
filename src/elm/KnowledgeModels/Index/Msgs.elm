module KnowledgeModels.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KnowledgeModels.Common.Models exposing (Package)


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
