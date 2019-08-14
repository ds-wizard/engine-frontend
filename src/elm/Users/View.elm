module Users.View exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html)
import Users.Create.View
import Users.Edit.View
import Users.Index.View
import Users.Models exposing (Model)
import Users.Msgs exposing (Msg(..))
import Users.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute ->
            Html.map CreateMsg <|
                Users.Create.View.view appState model.createModel

        EditRoute _ ->
            Html.map EditMsg <|
                Users.Edit.View.view appState model.editModel

        IndexRoute ->
            Html.map IndexMsg <|
                Users.Index.View.view appState model.indexModel
