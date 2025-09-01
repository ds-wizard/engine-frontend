module Wizard.Users.Msgs exposing (Msg(..))

import Wizard.Users.Create.Msgs
import Wizard.Users.Edit.Msgs
import Wizard.Users.Index.Msgs


type Msg
    = CreateMsg Wizard.Users.Create.Msgs.Msg
    | EditMsg Wizard.Users.Edit.Msgs.Msg
    | IndexMsg Wizard.Users.Index.Msgs.Msg
