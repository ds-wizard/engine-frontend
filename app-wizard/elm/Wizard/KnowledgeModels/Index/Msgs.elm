module Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.Package.PackagePhase exposing (PackagePhase)
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
