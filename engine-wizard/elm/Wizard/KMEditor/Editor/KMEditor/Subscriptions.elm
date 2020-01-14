module Wizard.KMEditor.Editor.KMEditor.Subscriptions exposing (subscriptions)

import Reorderable
import SplitPane
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (Msg(..))
import Wizard.Msgs


subscriptions : (Msg -> Wizard.Msgs.Msg) -> Model -> Sub Wizard.Msgs.Msg
subscriptions wrapMsg model =
    Sub.batch
        [ Reorderable.subscriptions model.reorderableState |> Sub.map (wrapMsg << ReorderableMsg)
        , SplitPane.subscriptions model.splitPane |> Sub.map (wrapMsg << PaneMsg)
        ]
