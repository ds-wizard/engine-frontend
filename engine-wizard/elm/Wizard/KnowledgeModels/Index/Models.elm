module Wizard.KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Package exposing (Package)
import Wizard.Common.Components.Listing as Listing


type alias Model =
    { packages : ActionResult (Listing.Model Package)
    , packageToBeDeleted : Maybe Package
    , deletingPackage : ActionResult String
    }


initialModel : Model
initialModel =
    { packages = Loading
    , packageToBeDeleted = Nothing
    , deletingPackage = Unset
    }
