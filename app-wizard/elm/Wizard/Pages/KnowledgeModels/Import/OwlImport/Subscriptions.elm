module Wizard.Pages.KnowledgeModels.Import.OwlImport.Subscriptions exposing (subscriptions)

import Wizard.Pages.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))
import Wizard.Ports.Import as Import


subscriptions : Sub Msg
subscriptions =
    Import.fileContentRead FileRead
