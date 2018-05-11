module KMEditor.Editor.Subscriptions exposing (subscriptions)

{-|

@docs subscriptions

-}

import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Msgs exposing (Msg(..))
import Msgs
import Reorderable


{-| -}
subscriptions : Model -> Sub Msgs.Msg
subscriptions model =
    Reorderable.subscriptions (ReorderableMsg >> Msgs.KnowledgeModelsEditorMsg) model.reorderableState
