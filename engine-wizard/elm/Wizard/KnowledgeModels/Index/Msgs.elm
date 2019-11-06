module Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.KnowledgeModels.Common.Package exposing (Package)


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
