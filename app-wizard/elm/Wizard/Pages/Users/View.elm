module Wizard.Pages.Users.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Create.View
import Wizard.Pages.Users.Edit.View
import Wizard.Pages.Users.Index.View
import Wizard.Pages.Users.Models exposing (Model)
import Wizard.Pages.Users.Msgs exposing (Msg(..))
import Wizard.Pages.Users.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Pages.Users.Create.View.view appState model.createModel

        EditRoute _ subroute ->
            Html.map EditMsg <|
                Wizard.Pages.Users.Edit.View.view appState subroute model.editModel

        IndexRoute _ _ ->
            Html.map IndexMsg <|
                Wizard.Pages.Users.Index.View.view appState model.indexModel
