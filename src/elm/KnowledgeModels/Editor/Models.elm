module KnowledgeModels.Editor.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Types exposing (ActionResult(..))
import KnowledgeModels.Editor.Models.Editors exposing (KnowledgeModelEditor)
import KnowledgeModels.Editor.Models.Events exposing (Event)
import Reorderable


{-| -}
type alias Model =
    { branchUuid : String
    , knowledgeModelEditor : ActionResult KnowledgeModelEditor
    , events : List Event
    , saving : ActionResult String
    , reorderableState : Reorderable.State
    }


{-| -}
initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , knowledgeModelEditor = Loading
    , events = []
    , saving = Unset
    , reorderableState = Reorderable.initialState
    }
