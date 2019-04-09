module KnowledgeModels.Subscriptions exposing (subscriptions)

import KnowledgeModels.Detail.Subscriptions
import KnowledgeModels.Import.Subscriptions
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Detail _ _ ->
            KnowledgeModels.Detail.Subscriptions.subscriptions (wrapMsg << DetailMsg) model.detailModel

        Import ->
            KnowledgeModels.Import.Subscriptions.subscriptions (wrapMsg << ImportMsg) model.importModel

        _ ->
            Sub.none
