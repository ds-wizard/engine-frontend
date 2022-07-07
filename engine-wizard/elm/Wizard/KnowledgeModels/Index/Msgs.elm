module Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Shared.Data.Package exposing (Package)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Package)
    | ExportPackage Package
