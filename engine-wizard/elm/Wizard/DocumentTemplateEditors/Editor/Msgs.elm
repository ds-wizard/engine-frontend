module Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))

import Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor as TemplateEditor


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDraftDetail)
    | TemplateEditorMsg TemplateEditor.Msg
    | FileEditorMsg FileEditor.Msg
    | PreviewMsg Preview.Msg
    | PublishModalMsg PublishModal.Msg
    | UpdatePreviewSettings DocumentTemplateDraftPreviewSettings
    | UpdateDocumentTemplate DocumentTemplateDraftDetail
    | Save
    | SaveForm
    | DiscardChanges
