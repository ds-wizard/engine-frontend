module Wizard.DocumentTemplateEditors.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Shared.Data.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { documentTemplateDrafts : Listing.Model DocumentTemplateDraft
    , documentTemplateDraftToBeDeleted : Maybe DocumentTemplateDraft
    , deletingDocumentTemplateDraft : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { documentTemplateDrafts = Listing.initialModel paginationQueryString
    , documentTemplateDraftToBeDeleted = Nothing
    , deletingDocumentTemplateDraft = ActionResult.Unset
    }
