module Msgs exposing (..)

import Auth.Msgs
import Navigation exposing (Location)
import Organization.Msgs
import PackageManagement.Detail.Msgs
import PackageManagement.Import.Msgs
import PackageManagement.Index.Msgs
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
    | OrganizationMsg Organization.Msgs.Msg
    | PackageManagementIndexMsg PackageManagement.Index.Msgs.Msg
    | PackageManagementDetailMsg PackageManagement.Detail.Msgs.Msg
    | PackageManagementImportMsg PackageManagement.Import.Msgs.Msg
