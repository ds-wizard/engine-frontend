module Questionnaires.Models exposing (Model, initLocalModel, initialModel)

import Questionnaires.Create.Models
import Questionnaires.CreateMigration.Models
import Questionnaires.Detail.Models
import Questionnaires.Edit.Models
import Questionnaires.Index.Models
import Questionnaires.Migration.Models
import Questionnaires.Routes exposing (Route(..))


type alias Model =
    { createModel : Questionnaires.Create.Models.Model
    , createMigrationModel : Questionnaires.CreateMigration.Models.Model
    , detailModel : Questionnaires.Detail.Models.Model
    , editModel : Questionnaires.Edit.Models.Model
    , indexModel : Questionnaires.Index.Models.Model
    , migrationModel : Questionnaires.Migration.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Questionnaires.Create.Models.initialModel Nothing
    , createMigrationModel = Questionnaires.CreateMigration.Models.initialModel ""
    , detailModel = Questionnaires.Detail.Models.initialModel ""
    , editModel = Questionnaires.Edit.Models.initialModel ""
    , indexModel = Questionnaires.Index.Models.initialModel
    , migrationModel = Questionnaires.Migration.Models.initialModel ""
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = Questionnaires.Create.Models.initialModel selectedPackage }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Questionnaires.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid ->
            { model | detailModel = Questionnaires.Detail.Models.initialModel uuid }

        EditRoute uuid ->
            { model | editModel = Questionnaires.Edit.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = Questionnaires.Index.Models.initialModel }

        MigrationRoute uuid ->
            { model | migrationModel = Questionnaires.Migration.Models.initialModel uuid }
