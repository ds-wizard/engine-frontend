module Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | ExportPackage PackageDetail
    | FileDownloaderMsg FileDownloader.Msg
