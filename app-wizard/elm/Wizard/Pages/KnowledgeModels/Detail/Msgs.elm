module Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal


type Msg
    = GetKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | DropdownMsg Dropdown.State
    | DeleteModalMsg DeleteModal.Msg
    | UpdatePhase KnowledgeModelPackagePhase
    | UpdatePhaseCompleted KnowledgeModelPackagePhase (Result ApiError ())
    | UpdatePublic Bool
    | UpdatePublicCompleted Bool (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackageDetail
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllVersions
