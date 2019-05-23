module KnowledgeModels.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import KnowledgeModels.Common.Package exposing (Package)


type alias Model =
    { packages : ActionResult (List Package)
    , packageToBeDeleted : Maybe Package
    , deletingPackage : ActionResult String
    }


initialModel : Model
initialModel =
    { packages = Loading
    , packageToBeDeleted = Nothing
    , deletingPackage = Unset
    }
