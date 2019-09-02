module KnowledgeModels.Import.FileImport.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.FileImport.Models exposing (Model)
import KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import Ports exposing (fileContentRead)


subscriptions : Model -> Sub Msg
subscriptions _ =
    fileContentRead FileRead
