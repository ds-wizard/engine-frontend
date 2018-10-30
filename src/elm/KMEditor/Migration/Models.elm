module KMEditor.Migration.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.Models.Migration exposing (Migration)


type alias Model =
    { branchUuid : String
    , migration : ActionResult Migration
    , conflict : ActionResult String
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , migration = Loading
    , conflict = Unset
    }
