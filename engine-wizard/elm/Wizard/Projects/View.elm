module Wizard.Projects.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.View
import Wizard.Projects.CreateMigration.View
import Wizard.Projects.Detail.View as Detail
import Wizard.Projects.DocumentDownload.View
import Wizard.Projects.FileDownload.View
import Wizard.Projects.Import.View
import Wizard.Projects.Index.View
import Wizard.Projects.Migration.View
import Wizard.Projects.Models exposing (Model)
import Wizard.Projects.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <|
                Wizard.Projects.Create.View.view appState model.createModel

        CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg <|
                Wizard.Projects.CreateMigration.View.view appState model.createMigrationModel

        DetailRoute _ subroute ->
            Html.map DetailMsg <|
                Detail.view subroute appState model.detailModel

        IndexRoute _ _ _ _ _ _ _ _ ->
            Html.map IndexMsg <|
                Wizard.Projects.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.Projects.Migration.View.view appState model.migrationModel

        ImportRoute _ _ ->
            Html.map ImportMsg <|
                Wizard.Projects.Import.View.view appState model.importModel

        DocumentDownloadRoute _ _ ->
            Html.map FileDownloadMsg <|
                Wizard.Projects.DocumentDownload.View.view appState model.documentDownload

        FileDownloadRoute _ _ ->
            Html.map FileDownloadMsg <|
                Wizard.Projects.FileDownload.View.view appState model.fileDownload
