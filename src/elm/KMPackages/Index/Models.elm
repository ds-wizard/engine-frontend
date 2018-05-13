module KMPackages.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import KMPackages.Common.Models exposing (Package)


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
