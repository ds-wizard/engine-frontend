module Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))

import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ShowHideDeleteDocumentTemplate (Maybe DocumentTemplate)
    | DeleteDocumentTemplate
    | DeleteDocumentTemplateCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplate)
    | ExportDocumentTemplate DocumentTemplate
    | FileDownloaderMsg FileDownloader.Msg
