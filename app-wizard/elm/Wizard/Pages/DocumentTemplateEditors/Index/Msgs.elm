module Wizard.Pages.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocumentTemplateDraft (Maybe DocumentTemplateDraft)
    | DeleteDocumentTemplateDraft
    | DeleteDocumentTemplateDraftCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg DocumentTemplateDraft)
