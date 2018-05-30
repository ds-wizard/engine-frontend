module KMEditor.Editor.Subscriptions exposing (subscriptions)

import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Msgs exposing (Msg(..))
import Msgs
import Reorderable


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    Reorderable.subscriptions (wrapMsg << ReorderableMsg) model.reorderableState
