module KMEditor.Editor.Subscriptions exposing (subscriptions)

import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Msgs exposing (Msg(..))
import Msgs
import Reorderable
import SplitPane


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    Sub.batch
        [ Reorderable.subscriptions model.reorderableState |> Sub.map (wrapMsg << ReorderableMsg)
        , SplitPane.subscriptions model.splitPane |> Sub.map (wrapMsg << PaneMsg)
        ]
