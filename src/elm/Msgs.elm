module Msgs exposing (..)

import Auth.Msgs
import KnowledgeModels.Create.Msgs
import KnowledgeModels.Editor.Msgs
import KnowledgeModels.Index.Msgs
import KnowledgeModels.Migration.Msgs
import KnowledgeModels.Publish.Msgs
import Navigation exposing (Location)
import Organization.Msgs
import PackageManagement.Detail.Msgs
import PackageManagement.Import.Msgs
import PackageManagement.Index.Msgs
import Public.Msgs
import UserManagement.Create.Msgs
import UserManagement.Edit.Msgs
import UserManagement.Index.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | UserManagementIndexMsg UserManagement.Index.Msgs.Msg
    | UserManagementCreateMsg UserManagement.Create.Msgs.Msg
    | UserManagementEditMsg UserManagement.Edit.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PackageManagementIndexMsg PackageManagement.Index.Msgs.Msg
    | PackageManagementDetailMsg PackageManagement.Detail.Msgs.Msg
    | PackageManagementImportMsg PackageManagement.Import.Msgs.Msg
    | KnowledgeModelsIndexMsg KnowledgeModels.Index.Msgs.Msg
    | KnowledgeModelsCreateMsg KnowledgeModels.Create.Msgs.Msg
    | KnowledgeModelsPublishMsg KnowledgeModels.Publish.Msgs.Msg
    | KnowledgeModelsEditorMsg KnowledgeModels.Editor.Msgs.Msg
    | KnowledgeModelsMigrationMsg KnowledgeModels.Migration.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
