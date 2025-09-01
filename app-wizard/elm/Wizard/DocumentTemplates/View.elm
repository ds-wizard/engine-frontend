module Wizard.DocumentTemplates.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplates.Detail.View
import Wizard.DocumentTemplates.Import.View
import Wizard.DocumentTemplates.Index.View
import Wizard.DocumentTemplates.Models exposing (Model)
import Wizard.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        DetailRoute _ ->
            Html.map DetailMsg <| Wizard.DocumentTemplates.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.DocumentTemplates.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.DocumentTemplates.Index.View.view appState model.indexModel
