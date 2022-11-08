module Wizard.DocumentTemplateEditors.Editor.Models exposing
    ( CurrentEditor(..)
    , Model
    , containsChanges
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor as TemplateEditor


type alias Model =
    { documentTemplateId : String
    , documentTemplate : ActionResult DocumentTemplateDraftDetail
    , currentEditor : CurrentEditor
    , templateEditorModel : TemplateEditor.Model
    , fileEditorModel : FileEditor.Model
    , previewModel : Preview.Model
    , publishModalModel : PublishModal.Model
    }


type CurrentEditor
    = TemplateEditor
    | FilesEditor
    | PreviewEditor


initialModel : String -> Model
initialModel documentTemplateId =
    { documentTemplateId = documentTemplateId
    , documentTemplate = ActionResult.Loading
    , currentEditor = TemplateEditor
    , templateEditorModel = TemplateEditor.initialModel
    , fileEditorModel = FileEditor.initialModel
    , previewModel = Preview.initialModel
    , publishModalModel = PublishModal.initialModel
    }


containsChanges : Model -> Bool
containsChanges model =
    FileEditor.filesChanged model.fileEditorModel || TemplateEditor.formChanged model.templateEditorModel
