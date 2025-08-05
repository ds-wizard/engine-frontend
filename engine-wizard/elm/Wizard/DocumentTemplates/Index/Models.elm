module Wizard.DocumentTemplates.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { documentTemplates : Listing.Model DocumentTemplate
    , documentTemplateToBeDeleted : Maybe DocumentTemplate
    , deletingDocumentTemplate : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { documentTemplates = Listing.initialModel paginationQueryString
    , documentTemplateToBeDeleted = Nothing
    , deletingDocumentTemplate = Unset
    }
