module Wizard.Pages.Tenants.Msgs exposing (Msg(..))

import Wizard.Pages.Tenants.Create.Msgs
import Wizard.Pages.Tenants.Detail.Msgs
import Wizard.Pages.Tenants.Index.Msgs


type Msg
    = IndexMsg Wizard.Pages.Tenants.Index.Msgs.Msg
    | CreateMsg Wizard.Pages.Tenants.Create.Msgs.Msg
    | DetailMsg Wizard.Pages.Tenants.Detail.Msgs.Msg
