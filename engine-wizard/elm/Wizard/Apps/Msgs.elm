module Wizard.Apps.Msgs exposing (Msg(..))

import Wizard.Apps.Create.Msgs
import Wizard.Apps.Detail.Msgs
import Wizard.Apps.Index.Msgs


type Msg
    = IndexMsg Wizard.Apps.Index.Msgs.Msg
    | CreateMsg Wizard.Apps.Create.Msgs.Msg
    | DetailMsg Wizard.Apps.Detail.Msgs.Msg
