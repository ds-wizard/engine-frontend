module Wizard.Pages.KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal


type alias Model =
    { packages : Listing.Model KnowledgeModelPackage
    , deleteModalModel : DeleteModal.Model
    , updatingKmPackagePhase : ActionResult ()
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { packages = Listing.initialModel paginationQueryString
    , deleteModalModel = DeleteModal.initialModel True
    , updatingKmPackagePhase = Unset
    }
