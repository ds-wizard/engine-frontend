module Wizard.Tenants.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Tenants.Create.View
import Wizard.Tenants.Detail.View
import Wizard.Tenants.Index.View
import Wizard.Tenants.Models exposing (Model)
import Wizard.Tenants.Msgs exposing (Msg(..))
import Wizard.Tenants.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        IndexRoute _ _ _ ->
            Html.map IndexMsg <|
                Wizard.Tenants.Index.View.view appState model.indexModel

        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Tenants.Create.View.view appState model.createModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Tenants.Detail.View.view appState model.detailModel
