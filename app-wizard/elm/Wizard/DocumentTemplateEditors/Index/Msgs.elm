module Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocumentTemplateDraft (Maybe DocumentTemplateDraft)
    | DeleteDocumentTemplateDraft
    | DeleteDocumentTemplateDraftCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplateDraft)
