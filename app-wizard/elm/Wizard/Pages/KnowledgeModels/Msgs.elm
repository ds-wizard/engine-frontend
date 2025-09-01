module Wizard.Pages.KnowledgeModels.Msgs exposing (Msg(..))

import Wizard.Pages.KnowledgeModels.Detail.Msgs
import Wizard.Pages.KnowledgeModels.Import.Msgs
import Wizard.Pages.KnowledgeModels.Index.Msgs
import Wizard.Pages.KnowledgeModels.Preview.Msgs
import Wizard.Pages.KnowledgeModels.ResourcePage.Msgs


type Msg
    = DetailMsg Wizard.Pages.KnowledgeModels.Detail.Msgs.Msg
    | ImportMsg Wizard.Pages.KnowledgeModels.Import.Msgs.Msg
    | IndexMsg Wizard.Pages.KnowledgeModels.Index.Msgs.Msg
    | PreviewMsg Wizard.Pages.KnowledgeModels.Preview.Msgs.Msg
    | ResourcePageMsg Wizard.Pages.KnowledgeModels.ResourcePage.Msgs.Msg
