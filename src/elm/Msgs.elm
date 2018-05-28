module Msgs exposing (..)

import Auth.Msgs
import DSPlanner.Msgs
import KMEditor.Create.Msgs
import KMEditor.Editor.Msgs
import KMEditor.Index.Msgs
import KMEditor.Migration.Msgs
import KMEditor.Publish.Msgs
import KMPackages.Msgs
import Navigation exposing (Location)
import Organization.Msgs
import Public.Msgs
import Users.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | DSPlannerMsg DSPlanner.Msgs.Msg
    | KMEditorCreateMsg KMEditor.Create.Msgs.Msg
    | KMEditorEditorMsg KMEditor.Editor.Msgs.Msg
    | KMEditorIndexMsg KMEditor.Index.Msgs.Msg
    | KMEditorMigrationMsg KMEditor.Migration.Msgs.Msg
    | KMEditorPublishMsg KMEditor.Publish.Msgs.Msg
    | KMPackagesMsg KMPackages.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | UsersMsg Users.Msgs.Msg
