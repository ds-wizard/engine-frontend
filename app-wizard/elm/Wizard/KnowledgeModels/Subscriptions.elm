module Wizard.KnowledgeModels.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Detail.Subscriptions
import Wizard.KnowledgeModels.Import.Subscriptions
import Wizard.KnowledgeModels.Index.Subscriptions
import Wizard.KnowledgeModels.Models exposing (Model)
import Wizard.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Preview.Subscriptions
import Wizard.KnowledgeModels.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ ->
            Sub.map DetailMsg <| Wizard.KnowledgeModels.Detail.Subscriptions.subscriptions model.detailModel

        ImportRoute _ ->
            Sub.map ImportMsg <| Wizard.KnowledgeModels.Import.Subscriptions.subscriptions model.importModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.KnowledgeModels.Index.Subscriptions.subscriptions model.indexModel

        PreviewRoute _ _ ->
            Sub.map PreviewMsg <| Wizard.KnowledgeModels.Preview.Subscriptions.subscriptions model.previewModel

        _ ->
            Sub.none
