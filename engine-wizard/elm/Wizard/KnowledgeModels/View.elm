module Wizard.KnowledgeModels.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Detail.View
import Wizard.KnowledgeModels.Import.View
import Wizard.KnowledgeModels.Index.View
import Wizard.KnowledgeModels.Models exposing (Model)
import Wizard.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Preview.View
import Wizard.KnowledgeModels.ResourcePage.View
import Wizard.KnowledgeModels.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        DetailRoute _ ->
            Html.map DetailMsg <| Wizard.KnowledgeModels.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.KnowledgeModels.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.KnowledgeModels.Index.View.view appState model.indexModel

        PreviewRoute _ _ ->
            Html.map PreviewMsg <| Wizard.KnowledgeModels.Preview.View.view appState model.previewModel

        ResourcePageRoute _ _ ->
            Html.map ResourcePageMsg <| Wizard.KnowledgeModels.ResourcePage.View.view appState model.resourcePageModel
