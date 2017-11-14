module PackageManagement.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import PackageManagement.Models exposing (Package)


type alias Model =
    { packages : ActionResult (List Package)
    }


initialModel : Model
initialModel =
    { packages = Loading
    }
