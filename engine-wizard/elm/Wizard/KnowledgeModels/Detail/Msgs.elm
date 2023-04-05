module Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.Package.PackagePhase exposing (PackagePhase)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)
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
