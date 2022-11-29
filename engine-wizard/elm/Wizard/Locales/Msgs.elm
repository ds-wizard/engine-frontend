module Wizard.Locales.Msgs exposing (Msg(..))

import Wizard.Locales.Create.Msgs
import Wizard.Locales.Detail.Msgs
import Wizard.Locales.Import.Msgs
import Wizard.Locales.Index.Msgs


type Msg
    = CreateMsg Wizard.Locales.Create.Msgs.Msg
    | DetailMsg Wizard.Locales.Detail.Msgs.Msg
    | ImportMsg Wizard.Locales.Import.Msgs.Msg
    | IndexMsg Wizard.Locales.Index.Msgs.Msg
