module KnowledgeModels.Editor.Subscriptions exposing (..)

import KnowledgeModels.Editor.Models exposing (Model)
import KnowledgeModels.Editor.Msgs exposing (Msg(..))
import Msgs
import Reorderable


subscriptions : Model -> Sub Msgs.Msg
subscriptions model =
    Reorderable.subscriptions (ReorderableMsg >> Msgs.KnowledgeModelsEditorMsg) model.reorderableState
