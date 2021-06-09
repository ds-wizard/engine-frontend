module Wizard.Projects.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.Models
import Wizard.Projects.CreateMigration.Models
import Wizard.Projects.Detail.Models as Detail
import Wizard.Projects.Index.Models
import Wizard.Projects.Migration.Models
import Wizard.Projects.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Projects.Create.Models.Model
    , createMigrationModel : Wizard.Projects.CreateMigration.Models.Model
    , detailModel : Detail.Model
    , indexModel : Wizard.Projects.Index.Models.Model
    , migrationModel : Wizard.Projects.Migration.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Projects.Create.Models.empty
    , createMigrationModel = Wizard.Projects.CreateMigration.Models.initialModel Uuid.nil
    , detailModel = Detail.init appState Uuid.nil
    , indexModel = Wizard.Projects.Index.Models.initialModel PaginationQueryString.empty Nothing
    , migrationModel = Wizard.Projects.Migration.Models.initialModel Uuid.nil
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute subroute ->
            { model | createModel = Wizard.Projects.Create.Models.initialModel appState subroute }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Wizard.Projects.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid subroute ->
            if uuid == model.detailModel.uuid then
                { model | detailModel = Detail.initPageModel subroute model.detailModel }

            else
                { model | detailModel = Detail.initPageModel subroute <| Detail.init appState uuid }

        IndexRoute paginationQueryString mbIsTemplate ->
            { model | indexModel = Wizard.Projects.Index.Models.initialModel paginationQueryString mbIsTemplate }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.Projects.Migration.Models.initialModel uuid }
