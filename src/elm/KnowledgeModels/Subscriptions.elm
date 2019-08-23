module KnowledgeModels.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.Subscriptions
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        ImportRoute _ ->
            Sub.map ImportMsg <| KnowledgeModels.Import.Subscriptions.subscriptions model.importModel

        _ ->
            Sub.none
