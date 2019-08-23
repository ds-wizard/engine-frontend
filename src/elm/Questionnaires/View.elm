module Questionnaires.View exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html)
import Questionnaires.Create.View
import Questionnaires.CreateMigration.View
import Questionnaires.Detail.View
import Questionnaires.Edit.View
import Questionnaires.Index.View
import Questionnaires.Migration.View
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ ->
            Html.map CreateMsg <|
                Questionnaires.Create.View.view appState model.createModel

        CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg <|
                Questionnaires.CreateMigration.View.view appState model.createMigrationModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Questionnaires.Detail.View.view appState model.detailModel

        EditRoute _ ->
            Html.map EditMsg <|
                Questionnaires.Edit.View.view appState model.editModel

        IndexRoute ->
            Html.map IndexMsg <|
                Questionnaires.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Questionnaires.Migration.View.view appState model.migrationModel
