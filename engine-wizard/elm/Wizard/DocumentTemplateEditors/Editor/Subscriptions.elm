module Wizard.DocumentTemplateEditors.Editor.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fileEditorSub =
            Sub.map FileEditorMsg <|
                FileEditor.subscriptions model.fileEditorModel

        previewSub =
            Sub.map PreviewMsg <|
                Preview.subscriptions model.previewModel
    in
    Sub.batch [ fileEditorSub, previewSub ]
