module KMEditor.Editor.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor.Models.Editors exposing (KnowledgeModelEditor)
import Reorderable


type alias Model =
    { branchUuid : String
    , knowledgeModelEditor : ActionResult KnowledgeModelEditor
    , events : List Event
    , saving : ActionResult String
    , reorderableState : Reorderable.State
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , knowledgeModelEditor = Loading
    , events = []
    , saving = Unset
    , reorderableState = Reorderable.initialState
    }
