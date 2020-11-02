module Wizard.Templates.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Template exposing (Template)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { templates : Listing.Model Template
    , templateToBeDeleted : Maybe Template
    , deletingTemplate : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { templates = Listing.initialModel paginationQueryString
    , templateToBeDeleted = Nothing
    , deletingTemplate = Unset
    }
