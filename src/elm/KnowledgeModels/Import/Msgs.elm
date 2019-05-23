module KnowledgeModels.Import.Msgs exposing (Msg(..))

import KnowledgeModels.Import.FileImport.Msgs as FileImportMsgs
import KnowledgeModels.Import.RegistryImport.Msgs as RegistryImportMsgs


type Msg
    = FileImportMsg FileImportMsgs.Msg
    | RegistryImportMsg RegistryImportMsgs.Msg
    | ShowRegistryImport
    | ShowFileImport
