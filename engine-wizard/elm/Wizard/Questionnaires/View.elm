module Wizard.Questionnaires.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Create.View
import Wizard.Questionnaires.CreateMigration.View
import Wizard.Questionnaires.Detail.View
import Wizard.Questionnaires.Edit.View
import Wizard.Questionnaires.Index.View
import Wizard.Questionnaires.Migration.View
import Wizard.Questionnaires.Models exposing (Model)
import Wizard.Questionnaires.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ ->
            Html.map CreateMsg <|
                Wizard.Questionnaires.Create.View.view appState model.createModel

        CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg <|
                Wizard.Questionnaires.CreateMigration.View.view appState model.createMigrationModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Questionnaires.Detail.View.view appState model.detailModel

        EditRoute _ ->
            Html.map EditMsg <|
                Wizard.Questionnaires.Edit.View.view appState model.editModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.Questionnaires.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.Questionnaires.Migration.View.view appState model.migrationModel
