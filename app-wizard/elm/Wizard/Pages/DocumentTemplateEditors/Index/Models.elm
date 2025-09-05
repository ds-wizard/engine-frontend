module Wizard.Pages.DocumentTemplateEditors.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Wizard.Components.Listing.Models as Listing


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
