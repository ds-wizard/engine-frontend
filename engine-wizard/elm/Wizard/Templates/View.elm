module Wizard.Templates.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Templates.Detail.View
import Wizard.Templates.Import.View
import Wizard.Templates.Index.View
import Wizard.Templates.Models exposing (Model)
import Wizard.Templates.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        DetailRoute _ ->
            Html.map DetailMsg <| Wizard.Templates.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.Templates.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.Templates.Index.View.view appState model.indexModel
