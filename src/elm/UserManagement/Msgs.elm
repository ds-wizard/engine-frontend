module UserManagement.Msgs exposing (..)

import UserManagement.Create.Msgs
import UserManagement.Edit.Msgs
import UserManagement.Index.Msgs


type Msg
    = CreateMsg UserManagement.Create.Msgs.Msg
    | EditMsg UserManagement.Edit.Msgs.Msg
    | IndexMsg UserManagement.Index.Msgs.Msg
