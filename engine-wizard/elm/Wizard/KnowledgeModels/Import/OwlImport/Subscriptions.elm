module Wizard.KnowledgeModels.Import.OwlImport.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Sub Msg
subscriptions =
    Ports.fileContentRead FileRead
