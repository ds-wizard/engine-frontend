module Wizard.Templates.Import.Msgs exposing (Msg(..))

import Wizard.Templates.Import.FileImport.Msgs as FileImportMsgs
import Wizard.Templates.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImportMsgs.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
