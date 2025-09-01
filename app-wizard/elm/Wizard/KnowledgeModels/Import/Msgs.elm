module Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))

import Wizard.Common.FileImport as FileImport
import Wizard.KnowledgeModels.Import.OwlImport.Msgs as OwlImportMsgs
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | OwlImportMsg OwlImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
    | ShowOwlImport
