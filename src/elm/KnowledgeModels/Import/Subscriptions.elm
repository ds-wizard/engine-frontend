module KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.Models exposing (Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import Msgs
import Ports exposing (fileContentRead)


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    fileContentRead (wrapMsg << FileRead)
