module Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Shared.Data.Package exposing (Package)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
