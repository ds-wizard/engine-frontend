module KnowledgeModels.Migration.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import KnowledgeModels.Models.Migration exposing (Migration)


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
