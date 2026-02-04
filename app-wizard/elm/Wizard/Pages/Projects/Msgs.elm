module Wizard.Pages.Projects.Msgs exposing (Msg(..))

import Wizard.Pages.Projects.Create.Msgs
import Wizard.Pages.Projects.CreateMigration.Msgs
import Wizard.Pages.Projects.Detail.Msgs as Detail
import Wizard.Pages.Projects.DocumentDownload.Msgs
import Wizard.Pages.Projects.FileDownload.Msgs
import Wizard.Pages.Projects.Import.Msgs
import Wizard.Pages.Projects.Index.Msgs
import Wizard.Pages.Projects.Migration.Msgs


type Msg
    = CreateMsg Wizard.Pages.Projects.Create.Msgs.Msg
    | CreateMigrationMsg Wizard.Pages.Projects.CreateMigration.Msgs.Msg
    | DetailMsg Detail.Msg
    | IndexMsg Wizard.Pages.Projects.Index.Msgs.Msg
    | MigrationMsg Wizard.Pages.Projects.Migration.Msgs.Msg
    | ImportMsg Wizard.Pages.Projects.Import.Msgs.Msg
    | DocumentDownloadMsg Wizard.Pages.Projects.DocumentDownload.Msgs.Msg
    | FileDownloadMsg Wizard.Pages.Projects.FileDownload.Msgs.Msg
