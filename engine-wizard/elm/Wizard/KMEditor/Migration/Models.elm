module Wizard.KMEditor.Migration.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.KMEditor.Common.Migration exposing (Migration)


type alias Model =
    { branchUuid : String
    , migration : ActionResult Migration
    , metrics : ActionResult (List Metric)
    , conflict : ActionResult String
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , migration = Loading
    , metrics = Loading
    , conflict = Unset
    }
