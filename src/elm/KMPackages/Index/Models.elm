module KMPackages.Index.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Types exposing (ActionResult(..))
import KMPackages.Models exposing (Package)


{-| -}
type alias Model =
    { packages : ActionResult (List Package)
    }


{-| -}
initialModel : Model
initialModel =
    { packages = Loading
    }
