module Msgs exposing (..)

import Auth.Msgs
import Navigation exposing (Location)
import UserManagement.Create.Msgs
import UserManagement.Delete.Msgs
import UserManagement.Edit.Msgs
import UserManagement.Index.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | UserManagementIndexMsg UserManagement.Index.Msgs.Msg
    | UserManagementCreateMsg UserManagement.Create.Msgs.Msg
    | UserManagementEditMsg UserManagement.Edit.Msgs.Msg
    | UserManagementDeleteMsg UserManagement.Delete.Msgs.Msg
