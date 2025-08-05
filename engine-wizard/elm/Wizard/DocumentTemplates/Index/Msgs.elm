module Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ShowHideDeleteDocumentTemplate (Maybe DocumentTemplate)
    | DeleteDocumentTemplate
    | DeleteDocumentTemplateCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplate)
    | UpdatePhase DocumentTemplate DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportDocumentTemplate DocumentTemplate
    | FileDownloaderMsg FileDownloader.Msg
