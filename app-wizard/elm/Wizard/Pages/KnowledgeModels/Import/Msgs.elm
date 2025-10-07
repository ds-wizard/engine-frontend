module Wizard.Pages.KnowledgeModels.Import.Msgs exposing (Msg(..))

import Wizard.Components.FileImport as FileImport
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Msgs as OwlImportMsgs
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | OwlImportMsg OwlImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
    | ShowOwlImport
