module Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.Package.PackagePhase exposing (PackagePhase)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | DropdownMsg Dropdown.State
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | UpdatePhase PackagePhase
    | UpdatePhaseCompleted PackagePhase (Result ApiError ())
    | ExportPackage PackageDetail
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllVersions
