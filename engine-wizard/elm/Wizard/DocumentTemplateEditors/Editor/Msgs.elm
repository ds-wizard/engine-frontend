module Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Prefab exposing (Prefab)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.Settings as Settings


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDraftDetail)
    | GetDocumentTemplateFormatPrefabsCompleted (Result ApiError (List (Prefab DocumentTemplateFormatDraft)))
    | GetDocumentTemplateFormatStepPrefabsCompleted (Result ApiError (List (Prefab DocumentTemplateFormatStep)))
    | SettingsMsg Settings.Msg
    | FileEditorMsg FileEditor.Msg
    | PreviewMsg Preview.Msg
    | PublishModalMsg PublishModal.Msg
    | UpdatePreviewSettings DocumentTemplateDraftPreviewSettings
    | UpdateDocumentTemplate DocumentTemplateDraftDetail
    | Save
    | SaveForm
    | DiscardChanges
