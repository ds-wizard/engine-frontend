module PackageManagement.Import.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Types exposing (ActionResult(..))
import FileReader exposing (NativeFile)


{-| -}
type alias Model =
    { dnd : Int
    , files : List NativeFile
    , importing : ActionResult String
    }


{-| -}
initialModel : Model
initialModel =
    { dnd = 0
    , files = []
    , importing = Unset
    }
