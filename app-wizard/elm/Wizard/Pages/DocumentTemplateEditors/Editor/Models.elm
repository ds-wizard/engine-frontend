module Wizard.Pages.DocumentTemplateEditors.Editor.Models exposing
    ( CurrentEditor(..)
    , Model
    , containsChanges
    , initialModel
    , setEditorFromRoute
    )

import ActionResult exposing (ActionResult)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.Settings as Settings
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute exposing (DTEditorRoute)


type alias Model =
    { documentTemplateId : String
    , documentTemplate : ActionResult DocumentTemplateDraftDetail
    , currentEditor : CurrentEditor
    , settingsModel : Settings.Model
    , fileEditorModel : FileEditor.Model
    , previewModel : Preview.Model
    , publishModalModel : PublishModal.Model
    , unloadMessageSet : Bool
    , documentTemplateFormatPrefabs : ActionResult (List DocumentTemplateFormatDraft)
    , documentTemplateFormatStepPrefabs : ActionResult (List DocumentTemplateFormatStep)
    }


type CurrentEditor
    = TemplateEditor
    | FilesEditor
    | PreviewEditor


initialModel : AppState -> String -> DTEditorRoute -> Model
initialModel appState documentTemplateId editorRoute =
    setEditorFromRoute editorRoute
        { documentTemplateId = documentTemplateId
        , documentTemplate = ActionResult.Loading
        , currentEditor = TemplateEditor
        , settingsModel = Settings.initialModel appState
        , fileEditorModel = FileEditor.initialModel
        , previewModel = Preview.initialModel
        , publishModalModel = PublishModal.initialModel
        , unloadMessageSet = False
        , documentTemplateFormatPrefabs = ActionResult.Loading
        , documentTemplateFormatStepPrefabs = ActionResult.Loading
        }


setEditorFromRoute : DTEditorRoute -> Model -> Model
setEditorFromRoute editorRoute model =
    let
        currentEditor =
            case editorRoute of
                DTEditorRoute.Settings ->
                    TemplateEditor

                DTEditorRoute.Files ->
                    FilesEditor

                DTEditorRoute.Preview ->
                    PreviewEditor
    in
    { model | currentEditor = currentEditor }


containsChanges : Model -> Bool
containsChanges model =
    FileEditor.filesChanged model.fileEditorModel || Settings.formChanged model.settingsModel
