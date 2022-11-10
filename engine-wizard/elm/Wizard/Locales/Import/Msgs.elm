module Wizard.Locales.Import.Msgs exposing (Msg(..))

import Wizard.Locales.Import.FileImport.Msgs as FileImportMsgs
import Wizard.Locales.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImportMsgs.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
