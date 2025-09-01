module Wizard.Pages.DocumentTemplates.Import.Msgs exposing (Msg(..))

import Wizard.Components.FileImport as FileImport
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
