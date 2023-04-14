module Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Shared.Data.Package exposing (Package)
import Shared.Data.Package.PackagePhase exposing (PackagePhase)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Package)
    | UpdatePhase Package PackagePhase
    | UpdatePhaseCompleted (Result ApiError ())
    | ExportPackage Package
    | FileDownloaderMsg FileDownloader.Msg
