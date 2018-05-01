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
import Questionnaires.Msgs
import UserManagement.Msgs


type Msg
    = ChangeLocation String
    | OnLocationChange Location
    | AuthMsg Auth.Msgs.Msg
    | OrganizationMsg Organization.Msgs.Msg
    | PackageManagementIndexMsg PackageManagement.Index.Msgs.Msg
    | PackageManagementDetailMsg PackageManagement.Detail.Msgs.Msg
    | PackageManagementImportMsg PackageManagement.Import.Msgs.Msg
    | KnowledgeModelsIndexMsg KnowledgeModels.Index.Msgs.Msg
    | KnowledgeModelsCreateMsg KnowledgeModels.Create.Msgs.Msg
    | KnowledgeModelsPublishMsg KnowledgeModels.Publish.Msgs.Msg
    | KnowledgeModelsEditorMsg KnowledgeModels.Editor.Msgs.Msg
    | KnowledgeModelsMigrationMsg KnowledgeModels.Migration.Msgs.Msg
    | QuestionnairesMsg Questionnaires.Msgs.Msg
    | PublicMsg Public.Msgs.Msg
    | UserManagementMsg UserManagement.Msgs.Msg
