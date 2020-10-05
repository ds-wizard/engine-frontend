module Wizard.Projects.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.View
import Wizard.Projects.CreateMigration.View
import Wizard.Projects.Detail.View as Detail
import Wizard.Projects.Index.View
import Wizard.Projects.Migration.View
import Wizard.Projects.Models exposing (Model)
import Wizard.Projects.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ ->
            Html.map CreateMsg <|
                Wizard.Projects.Create.View.view appState model.createModel

        CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg <|
                Wizard.Projects.CreateMigration.View.view appState model.createMigrationModel

        DetailRoute _ subroute ->
            Html.map DetailMsg <|
                Detail.view subroute appState model.detailModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.Projects.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.Projects.Migration.View.view appState model.migrationModel
