module Msgs exposing (Msg(..))

import Auth.Msgs
import Browser exposing (UrlRequest)
import Common.Menu.Msgs
import DSPlanner.Msgs
import KMEditor.Msgs
import KMPackages.Msgs
import Organization.Msgs
import Public.Msgs
import Url exposing (Url)
import Users.Msgs


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | AuthMsg Auth.Msgs.Msg
    | SetSidebarCollapsed Bool
    | MenuMsg Common.Menu.Msgs.Msg
    | DSPlannerMsg DSPlanner.Msgs.Msg
    | KMEditorMsg KMEditor.Msgs.Msg
    | KMPackagesMsg KMPackages.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | UsersMsg Users.Msgs.Msg
