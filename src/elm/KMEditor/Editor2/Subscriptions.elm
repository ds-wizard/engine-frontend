module KMEditor.Editor2.Subscriptions exposing (subscriptions)

import KMEditor.Editor2.KMEditor.Subscriptions
import KMEditor.Editor2.Models exposing (EditorType(..), Model)
import KMEditor.Editor2.Msgs exposing (Msg(..))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    case ( model.currentEditor, model.editorModel ) of
        ( KMEditor, Just editorModel ) ->
            KMEditor.Editor2.KMEditor.Subscriptions.subscriptions (wrapMsg << KMEditorMsg) editorModel

        _ ->
            Sub.none
