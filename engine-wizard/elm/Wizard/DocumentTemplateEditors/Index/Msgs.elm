module Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))

import Shared.Data.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocumentTemplateDraft (Maybe DocumentTemplateDraft)
    | DeleteDocumentTemplateDraft
    | DeleteDocumentTemplateDraftCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplateDraft)
