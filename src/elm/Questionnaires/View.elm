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
import Questionnaires.Routing exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        Create _ ->
            Html.map CreateMsg <|
                Questionnaires.Create.View.view appState model.createModel

        CreateMigration _ ->
            Html.map CreateMigrationMsg <|
                Questionnaires.CreateMigration.View.view model.createMigrationModel

        Detail _ ->
            Html.map DetailMsg <|
                Questionnaires.Detail.View.view appState model.detailModel

        Edit _ ->
            Html.map EditMsg <|
                Questionnaires.Edit.View.view appState model.editModel

        Index ->
            Html.map IndexMsg <|
                Questionnaires.Index.View.view appState model.indexModel

        Migration _ ->
            Html.map MigrationMsg <|
                Questionnaires.Migration.View.view appState model.migrationModel
