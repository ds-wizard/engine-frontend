module Wizard.KMEditor.Editor.Subscriptions exposing (subscriptions)

import Wizard.KMEditor.Editor.KMEditor.Subscriptions
import Wizard.KMEditor.Editor.Models exposing (EditorType(..), Model)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Editor.Preview.Subscriptions
import Wizard.Msgs


subscriptions : (Msg -> Wizard.Msgs.Msg) -> Model -> Sub Wizard.Msgs.Msg
subscriptions wrapMsg model =
    case ( model.currentEditor, model.editorModel, model.previewEditorModel ) of
        ( KMEditor, Just editorModel, _ ) ->
            Wizard.KMEditor.Editor.KMEditor.Subscriptions.subscriptions (wrapMsg << KMEditorMsg) editorModel

        ( PreviewEditor, _, Just previewEditorModel ) ->
            Sub.map (wrapMsg << PreviewEditorMsg) <|
                Wizard.KMEditor.Editor.Preview.Subscriptions.subscriptions previewEditorModel

        _ ->
            Sub.none
