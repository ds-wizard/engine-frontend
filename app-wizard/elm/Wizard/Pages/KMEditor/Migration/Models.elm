module Wizard.Pages.KMEditor.Migration.Models exposing
    ( ButtonClicked(..)
    , Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelMigration exposing (KnowledgeModelMigration)


type alias Model =
    { kmEditorUuid : Uuid
    , migration : ActionResult KnowledgeModelMigration
    , conflict : ActionResult String
    , buttonClicked : Maybe ButtonClicked
    }


type ButtonClicked
    = RejectButtonClicked
    | ApplyButtonClicked
    | ApplyAllButtonClicked


initialModel : Uuid -> Model
initialModel kmEditorUuid =
    { kmEditorUuid = kmEditorUuid
    , migration = Loading
    , conflict = Unset
    , buttonClicked = Nothing
    }
