module Msgs exposing (..)

import Auth.Msgs
import Common.Menu.Msgs
import DSPlanner.Msgs
import KMEditor.Msgs
import KMPackages.Msgs
import Navigation exposing (Location)
import Organization.Msgs
import Public.Msgs
import Users.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | SetSidebarCollapsed Bool
    | MenuMsg Common.Menu.Msgs.Msg
    | DSPlannerMsg DSPlanner.Msgs.Msg
    | KMEditorMsg KMEditor.Msgs.Msg
    | KMPackagesMsg KMPackages.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | UsersMsg Users.Msgs.Msg
