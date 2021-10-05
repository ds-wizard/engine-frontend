module Wizard.Projects.Subscriptions exposing (subscriptions)

import Wizard.Projects.Create.Subscriptions
import Wizard.Projects.CreateMigration.Subscriptions
import Wizard.Projects.Detail.Subscriptions
import Wizard.Projects.Index.Subscriptions
import Wizard.Projects.Models exposing (Model)
import Wizard.Projects.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        CreateRoute _ ->
            Sub.map CreateMsg <|
                Wizard.Projects.Create.Subscriptions.subscriptions model.createModel

        CreateMigrationRoute _ ->
            Sub.map CreateMigrationMsg <|
                Wizard.Projects.CreateMigration.Subscriptions.subscriptions model.createMigrationModel

        DetailRoute _ subroute ->
            Sub.map DetailMsg <|
                Wizard.Projects.Detail.Subscriptions.subscriptions subroute model.detailModel

        IndexRoute _ _ _ ->
            Sub.map IndexMsg <| Wizard.Projects.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
