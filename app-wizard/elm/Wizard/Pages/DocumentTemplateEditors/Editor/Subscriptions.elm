module Wizard.Pages.DocumentTemplateEditors.Editor.Subscriptions exposing (subscriptions)

import Time
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.Pages.DocumentTemplateEditors.Editor.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))


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
