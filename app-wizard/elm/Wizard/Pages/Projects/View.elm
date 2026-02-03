module Wizard.Pages.Projects.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Create.View
import Wizard.Pages.Projects.CreateMigration.View
import Wizard.Pages.Projects.Detail.View as Detail
import Wizard.Pages.Projects.DocumentDownload.View
import Wizard.Pages.Projects.FileDownload.View
import Wizard.Pages.Projects.Import.View
import Wizard.Pages.Projects.ImportLegacy.View
import Wizard.Pages.Projects.Index.View
import Wizard.Pages.Projects.Migration.View
import Wizard.Pages.Projects.Models exposing (Model)
import Wizard.Pages.Projects.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <|
                Wizard.Pages.Projects.Create.View.view appState model.createModel

        CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg <|
                Wizard.Pages.Projects.CreateMigration.View.view appState model.createMigrationModel

        DetailRoute _ subroute ->
            Html.map DetailMsg <|
                Detail.view subroute appState model.detailModel

        IndexRoute _ _ _ _ _ _ _ _ ->
            Html.map IndexMsg <|
                Wizard.Pages.Projects.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.Pages.Projects.Migration.View.view appState model.migrationModel

        ImportRoute _ _ ->
            Html.map ImportMsg <|
                Wizard.Pages.Projects.Import.View.view appState model.importModel

        ImportLegacyRoute _ _ ->
            Html.map ImportLegacyMsg <|
                Wizard.Pages.Projects.ImportLegacy.View.view appState model.importLegacyModel

        DocumentDownloadRoute _ _ ->
            Html.map FileDownloadMsg <|
                Wizard.Pages.Projects.DocumentDownload.View.view appState model.documentDownload

        FileDownloadRoute _ _ ->
            Html.map FileDownloadMsg <|
                Wizard.Pages.Projects.FileDownload.View.view appState model.fileDownload
