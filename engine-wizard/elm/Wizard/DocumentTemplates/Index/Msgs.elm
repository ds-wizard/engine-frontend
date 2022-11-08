module Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))

import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocumentTemplate (Maybe DocumentTemplate)
    | DeleteDocumentTemplate
    | DeleteDocumentTemplateCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplate)
    | ExportDocumentTemplate DocumentTemplate
