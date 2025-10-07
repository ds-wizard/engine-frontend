module Wizard.Pages.DocumentTemplates.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocumentTemplate (Maybe DocumentTemplate)
    | DeleteDocumentTemplate
    | DeleteDocumentTemplateCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplate)
    | UpdatePhase DocumentTemplate DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportDocumentTemplate DocumentTemplate
    | FileDownloaderMsg FileDownloader.Msg
