module Wizard.Pages.DocumentTemplateEditors.Msgs exposing (Msg(..))

import Wizard.Pages.DocumentTemplateEditors.Create.Msgs
import Wizard.Pages.DocumentTemplateEditors.Editor.Msgs
import Wizard.Pages.DocumentTemplateEditors.Index.Msgs


type Msg
    = CreateMsg Wizard.Pages.DocumentTemplateEditors.Create.Msgs.Msg
    | IndexMsg Wizard.Pages.DocumentTemplateEditors.Index.Msgs.Msg
    | EditorMsg Wizard.Pages.DocumentTemplateEditors.Editor.Msgs.Msg
