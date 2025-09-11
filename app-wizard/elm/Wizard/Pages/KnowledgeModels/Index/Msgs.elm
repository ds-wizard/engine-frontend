module Wizard.Pages.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.Package.PackagePhase exposing (PackagePhase)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Package)
    | UpdatePhase Package PackagePhase
    | UpdatePhaseCompleted (Result ApiError ())
    | ExportPackage Package
    | FileDownloaderMsg FileDownloader.Msg
