module Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))

import Wizard.DocumentTemplateEditors.Create.Msgs
import Wizard.DocumentTemplateEditors.Editor.Msgs
import Wizard.DocumentTemplateEditors.Index.Msgs


type Msg
    = CreateMsg Wizard.DocumentTemplateEditors.Create.Msgs.Msg
    | IndexMsg Wizard.DocumentTemplateEditors.Index.Msgs.Msg
    | EditorMsg Wizard.DocumentTemplateEditors.Editor.Msgs.Msg
