module Wizard.KMEditor.Editor.Subscriptions exposing (subscriptions)

import Wizard.KMEditor.Editor.KMEditor.Subscriptions
import Wizard.KMEditor.Editor.Models exposing (EditorType(..), Model)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.Msgs


subscriptions : (Msg -> Wizard.Msgs.Msg) -> Model -> Sub Wizard.Msgs.Msg
subscriptions wrapMsg model =
    case ( model.currentEditor, model.editorModel ) of
        ( KMEditor, Just editorModel ) ->
            Wizard.KMEditor.Editor.KMEditor.Subscriptions.subscriptions (wrapMsg << KMEditorMsg) editorModel

        _ ->
            Sub.none
