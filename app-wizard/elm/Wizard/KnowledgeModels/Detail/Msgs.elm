module Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Package.PackagePhase exposing (PackagePhase)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Common.FileDownloader as FileDownloader


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
