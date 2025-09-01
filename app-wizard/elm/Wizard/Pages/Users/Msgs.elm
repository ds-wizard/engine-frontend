module Wizard.Pages.Users.Msgs exposing (Msg(..))

import Wizard.Pages.Users.Create.Msgs
import Wizard.Pages.Users.Edit.Msgs
import Wizard.Pages.Users.Index.Msgs


type Msg
    = CreateMsg Wizard.Pages.Users.Create.Msgs.Msg
    | EditMsg Wizard.Pages.Users.Edit.Msgs.Msg
    | IndexMsg Wizard.Pages.Users.Index.Msgs.Msg
