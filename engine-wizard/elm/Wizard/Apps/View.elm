module Wizard.Apps.View exposing (view)

import Html exposing (Html)
import Wizard.Apps.Create.View
import Wizard.Apps.Detail.View
import Wizard.Apps.Index.View
import Wizard.Apps.Models exposing (Model)
import Wizard.Apps.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (Route(..))
import Wizard.Common.AppState exposing (AppState)


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        IndexRoute _ _ ->
            Html.map IndexMsg <|
                Wizard.Apps.Index.View.view appState model.indexModel

        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Apps.Create.View.view appState model.createModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Apps.Detail.View.view appState model.detailModel
