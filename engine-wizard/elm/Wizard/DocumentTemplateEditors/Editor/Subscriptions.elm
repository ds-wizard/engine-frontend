module Wizard.DocumentTemplateEditors.Editor.Subscriptions exposing (subscriptions)

import Time
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))


subscriptions : (Msg -> msg) -> (Time.Posix -> msg) -> Model -> Sub msg
subscriptions wrapMsg onTime model =
    let
        fileEditorSub =
            FileEditor.subscriptions (wrapMsg << FileEditorMsg) onTime model.fileEditorModel

        previewSub =
            Sub.map (wrapMsg << PreviewMsg) <|
                Preview.subscriptions model.previewModel
    in
    Sub.batch [ fileEditorSub, previewSub ]
