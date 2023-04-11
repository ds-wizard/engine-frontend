module Wizard.DocumentTemplateEditors.Editor.Models exposing
    ( CurrentEditor(..)
    , Model
    , containsChanges
    , initialModel
    , setEditorFromRoute
    )

import ActionResult exposing (ActionResult)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.Settings as Settings
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute exposing (DTEditorRoute)


type alias Model =
    { documentTemplateId : String
    , documentTemplate : ActionResult DocumentTemplateDraftDetail
    , currentEditor : CurrentEditor
    , settingsModel : Settings.Model
    , fileEditorModel : FileEditor.Model
    , previewModel : Preview.Model
    , publishModalModel : PublishModal.Model
    , unloadMessageSet : Bool
    }


type CurrentEditor
    = TemplateEditor
    | FilesEditor
    | PreviewEditor


initialModel : String -> DTEditorRoute -> Model
initialModel documentTemplateId editorRoute =
    setEditorFromRoute editorRoute
        { documentTemplateId = documentTemplateId
        , documentTemplate = ActionResult.Loading
        , currentEditor = TemplateEditor
        , settingsModel = Settings.initialModel
        , fileEditorModel = FileEditor.initialModel
        , previewModel = Preview.initialModel
        , publishModalModel = PublishModal.initialModel
        , unloadMessageSet = False
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
