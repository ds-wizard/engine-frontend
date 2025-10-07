module Wizard.Pages.Tenants.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Tenants.Create.View
import Wizard.Pages.Tenants.Detail.View
import Wizard.Pages.Tenants.Index.View
import Wizard.Pages.Tenants.Models exposing (Model)
import Wizard.Pages.Tenants.Msgs exposing (Msg(..))
import Wizard.Pages.Tenants.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        IndexRoute _ _ _ ->
            Html.map IndexMsg <|
                Wizard.Pages.Tenants.Index.View.view appState model.indexModel

        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Pages.Tenants.Create.View.view appState model.createModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Pages.Tenants.Detail.View.view appState model.detailModel
