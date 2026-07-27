module Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal
import Wizard.Pages.KnowledgeModels.Detail.ImportLocaleModal as ImportLocaleModal
import Wizard.Pages.KnowledgeModels.Detail.KnowledgeModelDetailRoute exposing (KnowledgeModelDetailRoute)


type Msg
    = GetKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | OpenDetailRoute KnowledgeModelDetailRoute
    | OpenImportLocaleModal
    | ImportLocaleModalMsg ImportLocaleModal.Msg
    | ShowDeleteLocale KnowledgeModelLocale
    | HideDeleteLocale
    | DeleteLocale
    | DeleteLocaleCompleted (Result ApiError ())
    | DownloadLocale KnowledgeModelLocale
    | DownloadLocaleCompleted KnowledgeModelLocale (Result ApiError String)
    | DropdownMsg Dropdown.State
    | DeleteModalMsg DeleteModal.Msg
    | UpdatePhase KnowledgeModelPackagePhase
    | UpdatePhaseCompleted KnowledgeModelPackagePhase (Result ApiError ())
    | UpdatePublic Bool
    | UpdatePublicCompleted Bool (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackageDetail
    | ExportKnowledgeModelPackagePot KnowledgeModelPackageDetail
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllVersions
