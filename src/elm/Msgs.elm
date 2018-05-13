module Msgs exposing (..)

import Auth.Msgs
import DSPlanner.Msgs
import KMEditor.Create.Msgs
import KMEditor.Editor.Msgs
import KMEditor.Index.Msgs
import KMEditor.Migration.Msgs
import KMEditor.Publish.Msgs
import KMPackages.Detail.Msgs
import KMPackages.Import.Msgs
import KMPackages.Index.Msgs
import KMPackages.Msgs
import Navigation exposing (Location)
import Organization.Msgs
import Public.Msgs
import Users.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | KMPackagesMsg KMPackages.Msgs.Msg
    | KnowledgeModelsIndexMsg KMEditor.Index.Msgs.Msg
    | KnowledgeModelsCreateMsg KMEditor.Create.Msgs.Msg
    | KnowledgeModelsPublishMsg KMEditor.Publish.Msgs.Msg
    | KnowledgeModelsEditorMsg KMEditor.Editor.Msgs.Msg
    | KnowledgeModelsMigrationMsg KMEditor.Migration.Msgs.Msg
    | QuestionnairesMsg DSPlanner.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | UserManagementMsg Users.Msgs.Msg
