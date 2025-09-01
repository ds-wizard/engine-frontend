module Wizard.DocumentTemplates.Import.Msgs exposing (Msg(..))

import Wizard.Common.FileImport as FileImport
import Wizard.DocumentTemplates.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
