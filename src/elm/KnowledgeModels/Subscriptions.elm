module KnowledgeModels.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.Subscriptions
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        Import _ ->
            Sub.map ImportMsg <| KnowledgeModels.Import.Subscriptions.subscriptions model.importModel

        _ ->
            Sub.none
