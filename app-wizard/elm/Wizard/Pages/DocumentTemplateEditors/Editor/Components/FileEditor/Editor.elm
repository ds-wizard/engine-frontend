module Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor.Editor exposing (Editor(..))

import Wizard.Api.Models.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)


type Editor
    = Empty
    | File DocumentTemplateFile
    | Asset DocumentTemplateAsset
