module Wizard.Users.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Users.Create.View
import Wizard.Users.Edit.View
import Wizard.Users.Index.View
import Wizard.Users.Models exposing (Model)
import Wizard.Users.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Users.Create.View.view appState model.createModel

        EditRoute _ subroute ->
            Html.map EditMsg <|
                Wizard.Users.Edit.View.view appState subroute model.editModel

        IndexRoute _ _ ->
            Html.map IndexMsg <|
                Wizard.Users.Index.View.view appState model.indexModel
