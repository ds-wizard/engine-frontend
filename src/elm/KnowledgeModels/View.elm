module KnowledgeModels.View exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html)
import KnowledgeModels.Detail.View
import KnowledgeModels.Import.View
import KnowledgeModels.Index.View
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        Detail _ ->
            Html.map DetailMsg <| KnowledgeModels.Detail.View.view appState model.detailModel

        Import _ ->
            Html.map ImportMsg <| KnowledgeModels.Import.View.view model.importModel

        Index ->
            Html.map IndexMsg <| KnowledgeModels.Index.View.view appState model.indexModel
