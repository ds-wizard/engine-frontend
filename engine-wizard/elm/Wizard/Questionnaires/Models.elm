module Wizard.Questionnaires.Models exposing (Model, initLocalModel, initialModel)

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


initialModel : Model
initialModel =
    { createModel = Wizard.Questionnaires.Create.Models.initialModel Nothing
    , createMigrationModel = Wizard.Questionnaires.CreateMigration.Models.initialModel ""
    , detailModel = Wizard.Questionnaires.Detail.Models.initialModel ""
    , editModel = Wizard.Questionnaires.Edit.Models.initialModel ""
    , indexModel = Wizard.Questionnaires.Index.Models.initialModel
    , migrationModel = Wizard.Questionnaires.Migration.Models.initialModel ""
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = Wizard.Questionnaires.Create.Models.initialModel selectedPackage }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Wizard.Questionnaires.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid ->
            { model | detailModel = Wizard.Questionnaires.Detail.Models.initialModel uuid }

        EditRoute uuid ->
            { model | editModel = Wizard.Questionnaires.Edit.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = Wizard.Questionnaires.Index.Models.initialModel }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.Questionnaires.Migration.Models.initialModel uuid }
