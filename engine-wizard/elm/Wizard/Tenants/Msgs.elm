module Wizard.Tenants.Msgs exposing (Msg(..))

import Wizard.Tenants.Create.Msgs
import Wizard.Tenants.Detail.Msgs
import Wizard.Tenants.Index.Msgs


type Msg
    = IndexMsg Wizard.Tenants.Index.Msgs.Msg
    | CreateMsg Wizard.Tenants.Create.Msgs.Msg
    | DetailMsg Wizard.Tenants.Detail.Msgs.Msg
