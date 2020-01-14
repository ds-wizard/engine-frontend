module Wizard.KnowledgeModels.Msgs exposing (Msg(..))

import Wizard.KnowledgeModels.Detail.Msgs
import Wizard.KnowledgeModels.Import.Msgs
import Wizard.KnowledgeModels.Index.Msgs


type Msg
    = DetailMsg Wizard.KnowledgeModels.Detail.Msgs.Msg
    | ImportMsg Wizard.KnowledgeModels.Import.Msgs.Msg
    | IndexMsg Wizard.KnowledgeModels.Index.Msgs.Msg
