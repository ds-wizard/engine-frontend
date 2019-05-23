module KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.Models exposing (Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import Ports exposing (fileContentRead)


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileRead
