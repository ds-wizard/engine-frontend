module KnowledgeModels.Msgs exposing (Msg(..))

import KnowledgeModels.Detail.Msgs
import KnowledgeModels.Import.Msgs
import KnowledgeModels.Index.Msgs


type Msg
    = DetailMsg KnowledgeModels.Detail.Msgs.Msg
    | ImportMsg KnowledgeModels.Import.Msgs.Msg
    | IndexMsg KnowledgeModels.Index.Msgs.Msg
