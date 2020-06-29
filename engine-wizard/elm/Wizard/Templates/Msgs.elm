module Wizard.Templates.Msgs exposing (Msg(..))

import Wizard.Templates.Detail.Msgs
import Wizard.Templates.Import.Msgs
import Wizard.Templates.Index.Msgs


type Msg
    = DetailMsg Wizard.Templates.Detail.Msgs.Msg
    | ImportMsg Wizard.Templates.Import.Msgs.Msg
    | IndexMsg Wizard.Templates.Index.Msgs.Msg
