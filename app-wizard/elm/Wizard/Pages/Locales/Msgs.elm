module Wizard.Pages.Locales.Msgs exposing (Msg(..))

import Wizard.Pages.Locales.Create.Msgs
import Wizard.Pages.Locales.Detail.Msgs
import Wizard.Pages.Locales.Import.Msgs
import Wizard.Pages.Locales.Index.Msgs


type Msg
    = CreateMsg Wizard.Pages.Locales.Create.Msgs.Msg
    | DetailMsg Wizard.Pages.Locales.Detail.Msgs.Msg
    | ImportMsg Wizard.Pages.Locales.Import.Msgs.Msg
    | IndexMsg Wizard.Pages.Locales.Index.Msgs.Msg
