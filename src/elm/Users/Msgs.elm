module Users.Msgs exposing (Msg(..))

import Users.Create.Msgs
import Users.Edit.Msgs
import Users.Index.Msgs


type Msg
    = CreateMsg Users.Create.Msgs.Msg
    | EditMsg Users.Edit.Msgs.Msg
    | IndexMsg Users.Index.Msgs.Msg
