module Wizard.Pages.KnowledgeModels.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Detail.View
import Wizard.Pages.KnowledgeModels.Import.View
import Wizard.Pages.KnowledgeModels.Index.View
import Wizard.Pages.KnowledgeModels.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Preview.View
import Wizard.Pages.KnowledgeModels.ResourcePage.View
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        DetailRoute _ ->
            Html.map DetailMsg <| Wizard.Pages.KnowledgeModels.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.Pages.KnowledgeModels.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.Pages.KnowledgeModels.Index.View.view appState model.indexModel

        PreviewRoute _ _ ->
            Html.map PreviewMsg <| Wizard.Pages.KnowledgeModels.Preview.View.view appState model.previewModel

        ResourcePageRoute _ _ ->
            Html.map ResourcePageMsg <| Wizard.Pages.KnowledgeModels.ResourcePage.View.view appState model.resourcePageModel
