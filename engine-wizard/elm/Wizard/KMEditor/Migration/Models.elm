module Wizard.KMEditor.Migration.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Migration exposing (Migration)
import Uuid exposing (Uuid)


type alias Model =
    { branchUuid : Uuid
    , migration : ActionResult Migration
    , conflict : ActionResult String
    }


initialModel : Uuid -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , migration = Loading
    , conflict = Unset
    }
