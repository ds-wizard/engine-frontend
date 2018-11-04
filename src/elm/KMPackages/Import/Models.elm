module KMPackages.Import.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
-- import FileReader exposing (NativeFile)


type alias Model =
    { dnd : Int
    -- , files : List NativeFile
    , importing : ActionResult String
    }


initialModel : Model
initialModel =
    { dnd = 0
    -- , files = []
    , importing = Unset
    }
