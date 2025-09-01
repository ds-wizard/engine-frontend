module Wizard.Pages.DocumentTemplates.Msgs exposing (Msg(..))

import Wizard.Pages.DocumentTemplates.Detail.Msgs
import Wizard.Pages.DocumentTemplates.Import.Msgs
import Wizard.Pages.DocumentTemplates.Index.Msgs


type Msg
    = DetailMsg Wizard.Pages.DocumentTemplates.Detail.Msgs.Msg
    | ImportMsg Wizard.Pages.DocumentTemplates.Import.Msgs.Msg
    | IndexMsg Wizard.Pages.DocumentTemplates.Index.Msgs.Msg
