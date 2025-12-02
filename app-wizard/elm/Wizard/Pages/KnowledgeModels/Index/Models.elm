module Wizard.Pages.KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { packages : Listing.Model KnowledgeModelPackage
    , kmPackageToBeDeleted : Maybe KnowledgeModelPackage
    , deletingKmPackage : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { packages = Listing.initialModel paginationQueryString
    , kmPackageToBeDeleted = Nothing
    , deletingKmPackage = Unset
    }
