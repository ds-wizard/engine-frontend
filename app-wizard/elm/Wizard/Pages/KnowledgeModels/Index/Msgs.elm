module Wizard.Pages.KnowledgeModels.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Components.Listing.Msgs as Listing
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal


type Msg
    = DeleteModalMsg DeleteModal.Msg
    | ListingMsg (Listing.Msg KnowledgeModelPackage)
    | UpdatePhase KnowledgeModelPackage KnowledgeModelPackagePhase
    | UpdatePhaseCompleted (Result ApiError ())
    | UpdatePublic KnowledgeModelPackage Bool
    | UpdatePublicCompleted (Result ApiError ())
    | ExportKnowledgeModelPackage KnowledgeModelPackage
    | FileDownloaderMsg FileDownloader.Msg
