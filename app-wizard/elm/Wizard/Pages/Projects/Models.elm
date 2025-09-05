module Wizard.Pages.Projects.Models exposing (Model, initLocalModel, initialModel)

import Common.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Create.Models
import Wizard.Pages.Projects.CreateMigration.Models
import Wizard.Pages.Projects.Detail.Models as Detail
import Wizard.Pages.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Pages.Projects.DocumentDownload.Models
import Wizard.Pages.Projects.FileDownload.Models
import Wizard.Pages.Projects.Import.Models
import Wizard.Pages.Projects.Index.Models
import Wizard.Pages.Projects.Migration.Models
import Wizard.Pages.Projects.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.Projects.Create.Models.Model
    , createMigrationModel : Wizard.Pages.Projects.CreateMigration.Models.Model
    , detailModel : Detail.Model
    , indexModel : Wizard.Pages.Projects.Index.Models.Model
    , migrationModel : Wizard.Pages.Projects.Migration.Models.Model
    , importModel : Wizard.Pages.Projects.Import.Models.Model
    , documentDownload : Wizard.Pages.Projects.DocumentDownload.Models.Model
    , fileDownload : Wizard.Pages.Projects.FileDownload.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Pages.Projects.Create.Models.initialModel appState Nothing Nothing
    , createMigrationModel = Wizard.Pages.Projects.CreateMigration.Models.initialModel Uuid.nil
    , detailModel = Detail.init appState Uuid.nil Nothing Nothing
    , indexModel = Wizard.Pages.Projects.Index.Models.initialModel PaginationQueryString.empty Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , migrationModel = Wizard.Pages.Projects.Migration.Models.initialModel Uuid.nil
    , importModel = Wizard.Pages.Projects.Import.Models.initialModel Uuid.nil ""
    , documentDownload = Wizard.Pages.Projects.DocumentDownload.Models.initialModel Uuid.nil Uuid.nil
    , fileDownload = Wizard.Pages.Projects.FileDownload.Models.initialModel Uuid.nil Uuid.nil
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedProjectTemplate selectedKnowledgeModel ->
            { model | createModel = Wizard.Pages.Projects.Create.Models.initialModel appState selectedProjectTemplate selectedKnowledgeModel }

        CreateMigrationRoute uuid ->
            { model | createMigrationModel = Wizard.Pages.Projects.CreateMigration.Models.initialModel uuid }

        DetailRoute uuid subroute ->
            if uuid == model.detailModel.uuid then
                { model | detailModel = Detail.initPageModel appState subroute model.detailModel }

            else
                let
                    ( mbSelectedPath, mbCommentThreadUuid ) =
                        case subroute of
                            ProjectDetailRoute.Questionnaire path commentThreadUuid ->
                                ( path, commentThreadUuid )

                            _ ->
                                ( Nothing, Nothing )
                in
                { model | detailModel = Detail.initPageModel appState subroute <| Detail.init appState uuid mbSelectedPath mbCommentThreadUuid }

        IndexRoute paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp ->
            { model | indexModel = Wizard.Pages.Projects.Index.Models.initialModel paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp (Just model.indexModel) }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.Pages.Projects.Migration.Models.initialModel uuid }

        ImportRoute uuid importerId ->
            { model | importModel = Wizard.Pages.Projects.Import.Models.initialModel uuid importerId }

        DocumentDownloadRoute questionnaireUuid fileUuid ->
            { model | documentDownload = Wizard.Pages.Projects.DocumentDownload.Models.initialModel questionnaireUuid fileUuid }

        FileDownloadRoute questionnaireUuid fileUuid ->
            { model | fileDownload = Wizard.Pages.Projects.FileDownload.Models.initialModel questionnaireUuid fileUuid }
