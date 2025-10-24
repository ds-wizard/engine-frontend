module Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)


type Msg
    = GetKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | DropdownMsg Dropdown.State
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | UpdatePhase KnowledgeModelPackagePhase
    | UpdatePhaseCompleted KnowledgeModelPackagePhase (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackageDetail
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllVersions
