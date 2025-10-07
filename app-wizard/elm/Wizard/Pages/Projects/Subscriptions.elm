module Wizard.Pages.Projects.Subscriptions exposing (subscriptions)

import Wizard.Pages.Projects.Create.Subscriptions
import Wizard.Pages.Projects.CreateMigration.Subscriptions
import Wizard.Pages.Projects.Detail.Subscriptions
import Wizard.Pages.Projects.Import.Subscriptions
import Wizard.Pages.Projects.Index.Subscriptions
import Wizard.Pages.Projects.Migration.Subscriptions
import Wizard.Pages.Projects.Models exposing (Model)
import Wizard.Pages.Projects.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        CreateRoute _ _ ->
            Sub.map CreateMsg <|
                Wizard.Pages.Projects.Create.Subscriptions.subscriptions model.createModel

        CreateMigrationRoute _ ->
            Sub.map CreateMigrationMsg <|
                Wizard.Pages.Projects.CreateMigration.Subscriptions.subscriptions model.createMigrationModel

        DetailRoute _ subroute ->
            Sub.map DetailMsg <|
                Wizard.Pages.Projects.Detail.Subscriptions.subscriptions subroute model.detailModel

        IndexRoute _ _ _ _ _ _ _ _ ->
            Sub.map IndexMsg <| Wizard.Pages.Projects.Index.Subscriptions.subscriptions model.indexModel

        MigrationRoute _ ->
            Sub.map MigrationMsg <| Wizard.Pages.Projects.Migration.Subscriptions.subscriptions model.migrationModel

        ImportRoute _ _ ->
            Sub.map ImportMsg <| Wizard.Pages.Projects.Import.Subscriptions.subscriptions model.importModel

        _ ->
            Sub.none
