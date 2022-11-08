module Wizard.DocumentTemplates.Import.Msgs exposing (Msg(..))

import Wizard.DocumentTemplates.Import.FileImport.Msgs as FileImportMsgs
import Wizard.DocumentTemplates.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImportMsgs.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
