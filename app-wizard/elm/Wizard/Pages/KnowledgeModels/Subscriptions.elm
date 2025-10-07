module Wizard.Pages.KnowledgeModels.Subscriptions exposing (subscriptions)

import Wizard.Pages.KnowledgeModels.Detail.Subscriptions
import Wizard.Pages.KnowledgeModels.Import.Subscriptions
import Wizard.Pages.KnowledgeModels.Index.Subscriptions
import Wizard.Pages.KnowledgeModels.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Preview.Subscriptions
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ ->
            Sub.map DetailMsg <| Wizard.Pages.KnowledgeModels.Detail.Subscriptions.subscriptions model.detailModel

        ImportRoute _ ->
            Sub.map ImportMsg <| Wizard.Pages.KnowledgeModels.Import.Subscriptions.subscriptions model.importModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Pages.KnowledgeModels.Index.Subscriptions.subscriptions model.indexModel

        PreviewRoute _ _ ->
            Sub.map PreviewMsg <| Wizard.Pages.KnowledgeModels.Preview.Subscriptions.subscriptions model.previewModel

        _ ->
            Sub.none
