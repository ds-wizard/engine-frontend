module KMEditor.Editor.Subscriptions exposing (subscriptions)

import KMEditor.Editor.KMEditor.Subscriptions
import KMEditor.Editor.Models exposing (EditorType(..), Model)
import KMEditor.Editor.Msgs exposing (Msg(..))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    case ( model.currentEditor, model.editorModel ) of
        ( KMEditor, Just editorModel ) ->
            KMEditor.Editor.KMEditor.Subscriptions.subscriptions (wrapMsg << KMEditorMsg) editorModel

        _ ->
            Sub.none
