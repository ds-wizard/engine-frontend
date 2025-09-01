module Wizard.DocumentTemplates.Msgs exposing (Msg(..))

import Wizard.DocumentTemplates.Detail.Msgs
import Wizard.DocumentTemplates.Import.Msgs
import Wizard.DocumentTemplates.Index.Msgs


type Msg
    = DetailMsg Wizard.DocumentTemplates.Detail.Msgs.Msg
    | ImportMsg Wizard.DocumentTemplates.Import.Msgs.Msg
    | IndexMsg Wizard.DocumentTemplates.Index.Msgs.Msg
