module Wizard.KnowledgeModels.Import.FileImport.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.FileImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.Ports as Ports exposing (fileContentRead)


subscriptions : Model -> Sub Msg
subscriptions _ =
    fileContentRead FileRead
