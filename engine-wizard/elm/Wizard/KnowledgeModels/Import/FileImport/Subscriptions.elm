module Wizard.KnowledgeModels.Import.FileImport.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.FileImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileContentRead FileRead
