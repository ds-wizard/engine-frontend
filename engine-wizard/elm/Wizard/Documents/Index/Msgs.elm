module Wizard.Documents.Index.Msgs exposing (..)

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing
import Wizard.Documents.Common.Document exposing (Document)


type Msg
    = GetDocumentsCompleted (Result ApiError (List Document))
    | ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
    | RefreshDocuments
    | RefreshDocumentsCompleted (Result ApiError (List Document))
