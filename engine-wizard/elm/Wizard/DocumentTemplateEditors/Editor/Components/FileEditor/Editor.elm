module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.Editor exposing (Editor(..))

import Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Data.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)


type Editor
    = Empty
    | File DocumentTemplateFile
    | Asset DocumentTemplateAsset
