module Wizard.KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.KnowledgeModels.Common.Package exposing (Package)


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
