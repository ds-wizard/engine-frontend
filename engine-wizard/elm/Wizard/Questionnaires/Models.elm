module Wizard.Questionnaires.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Create.Models
import Wizard.Questionnaires.CreateMigration.Models
import Wizard.Questionnaires.Detail.Models
import Wizard.Questionnaires.Edit.Models
import Wizard.Questionnaires.Index.Models
import Wizard.Questionnaires.Migration.Models
import Wizard.Questionnaires.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Questionnaires.Create.Models.Model
    , createMigrationModel : Wizard.Questionnaires.CreateMigration.Models.Model
    , detailModel : Wizard.Questionnaires.Detail.Models.Model
    , editModel : Wizard.Questionnaires.Edit.Models.Model
    , indexModel : Wizard.Questionnaires.Index.Models.Model
    , migrationModel : Wizard.Questionnaires.Migration.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Questionnaires.Create.Models.initialModel appState Nothing
    , createMigrationModel = Wizard.Questionnaires.CreateMigration.Models.initialModel Uuid.nil
    , detailModel = Wizard.Questionnaires.Detail.Models.initialModel appState Uuid.nil
    , editModel = Wizard.Questionnaires.Edit.Models.initialModel Uuid.nil
    , indexModel = Wizard.Questionnaires.Index.Models.initialModel PaginationQueryString.empty
    , migrationModel = Wizard.Questionnaires.Migration.Models.initialModel Uuid.nil
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = Wizard.Questionnaires.Create.Models.initialModel appState selectedPackage }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Wizard.Questionnaires.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid ->
            { model | detailModel = Wizard.Questionnaires.Detail.Models.initialModel appState uuid }

        EditRoute uuid ->
            { model | editModel = Wizard.Questionnaires.Edit.Models.initialModel uuid }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Questionnaires.Index.Models.initialModel paginationQueryString }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.Questionnaires.Migration.Models.initialModel uuid }
