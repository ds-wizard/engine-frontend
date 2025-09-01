module Wizard.KMEditor.Migration.Models exposing
    ( ButtonClicked(..)
    , Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Uuid exposing (Uuid)
import Wizard.Api.Models.Migration exposing (Migration)


type alias Model =
    { branchUuid : Uuid
    , migration : ActionResult Migration
    , conflict : ActionResult String
    , buttonClicked : Maybe ButtonClicked
    }


type ButtonClicked
    = RejectButtonClicked
    | ApplyButtonClicked
    | ApplyAllButtonClicked


initialModel : Uuid -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , migration = Loading
    , conflict = Unset
    , buttonClicked = Nothing
    }
