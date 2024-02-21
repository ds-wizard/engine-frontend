module Wizard.Projects.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.Models
import Wizard.Projects.CreateMigration.Models
import Wizard.Projects.Detail.Models as Detail
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Import.Models
import Wizard.Projects.Index.Models
import Wizard.Projects.Migration.Models
import Wizard.Projects.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Projects.Create.Models.Model
    , createMigrationModel : Wizard.Projects.CreateMigration.Models.Model
    , detailModel : Detail.Model
    , indexModel : Wizard.Projects.Index.Models.Model
    , migrationModel : Wizard.Projects.Migration.Models.Model
    , importModel : Wizard.Projects.Import.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Projects.Create.Models.initialModel appState Nothing Nothing
    , createMigrationModel = Wizard.Projects.CreateMigration.Models.initialModel Uuid.nil
    , detailModel = Detail.init appState Uuid.nil Nothing
    , indexModel = Wizard.Projects.Index.Models.initialModel PaginationQueryString.empty Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , migrationModel = Wizard.Projects.Migration.Models.initialModel Uuid.nil
    , importModel = Wizard.Projects.Import.Models.initialModel Uuid.nil ""
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedProjectTemplate selectedKnowledgeModel ->
            { model | createModel = Wizard.Projects.Create.Models.initialModel appState selectedProjectTemplate selectedKnowledgeModel }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Wizard.Projects.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid subroute ->
            if uuid == model.detailModel.uuid then
                { model | detailModel = Detail.initPageModel appState subroute model.detailModel }

            else
                let
                    mbSelectedPath =
                        case subroute of
                            ProjectDetailRoute.Questionnaire path ->
                                path

                            _ ->
                                Nothing
                in
                { model | detailModel = Detail.initPageModel appState subroute <| Detail.init appState uuid mbSelectedPath }

        IndexRoute paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp ->
            { model | indexModel = Wizard.Projects.Index.Models.initialModel paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp (Just model.indexModel) }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.Projects.Migration.Models.initialModel uuid }

        ImportRoute uuid importerId ->
            { model | importModel = Wizard.Projects.Import.Models.initialModel uuid importerId }
