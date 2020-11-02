module Wizard.KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Package exposing (Package)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { packages : Listing.Model Package
    , packageToBeDeleted : Maybe Package
    , deletingPackage : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { packages = Listing.initialModel paginationQueryString
    , packageToBeDeleted = Nothing
    , deletingPackage = Unset
    }
