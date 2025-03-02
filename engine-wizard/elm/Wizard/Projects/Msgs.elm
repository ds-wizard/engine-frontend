module Wizard.Projects.Msgs exposing (Msg(..))

import Wizard.Projects.Create.Msgs
import Wizard.Projects.CreateMigration.Msgs
import Wizard.Projects.Detail.Msgs as Detail
import Wizard.Projects.DocumentDownload.Msgs
import Wizard.Projects.FileDownload.Msgs
import Wizard.Projects.Import.Msgs
import Wizard.Projects.Index.Msgs
import Wizard.Projects.Migration.Msgs


type Msg
    = CreateMsg Wizard.Projects.Create.Msgs.Msg
    | CreateMigrationMsg Wizard.Projects.CreateMigration.Msgs.Msg
    | DetailMsg Detail.Msg
    | IndexMsg Wizard.Projects.Index.Msgs.Msg
    | MigrationMsg Wizard.Projects.Migration.Msgs.Msg
    | ImportMsg Wizard.Projects.Import.Msgs.Msg
    | DocumentDownloadMsg Wizard.Projects.DocumentDownload.Msgs.Msg
    | FileDownloadMsg Wizard.Projects.FileDownload.Msgs.Msg
