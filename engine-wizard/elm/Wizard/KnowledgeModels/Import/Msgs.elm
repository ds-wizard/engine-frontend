module Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))

import Wizard.KnowledgeModels.Import.FileImport.Msgs as FileImportMsgs
import Wizard.KnowledgeModels.Import.OwlImport.Msgs as OwlImportMsgs
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImportMsgs.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | OwlImportMsg OwlImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
    | ShowOwlImport
