module Wizard.KMEditor.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Create.Models
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Editor.Models as Editor
import Wizard.KMEditor.Index.Models
import Wizard.KMEditor.Migration.Models
import Wizard.KMEditor.Publish.Models
import Wizard.KMEditor.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.KMEditor.Create.Models.Model
    , editorModel : Editor.Model
    , indexModel : Wizard.KMEditor.Index.Models.Model
    , migrationModel : Wizard.KMEditor.Migration.Models.Model
    , publishModel : Wizard.KMEditor.Publish.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.KMEditor.Create.Models.initialModel Nothing Nothing
    , editorModel = Editor.init appState Uuid.nil Nothing
    , indexModel = Wizard.KMEditor.Index.Models.initialModel PaginationQueryString.empty
    , migrationModel = Wizard.KMEditor.Migration.Models.initialModel Uuid.nil
    , publishModel = Wizard.KMEditor.Publish.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedPackage edit ->
            { model | createModel = Wizard.KMEditor.Create.Models.initialModel selectedPackage edit }

        EditorRoute uuid subroute ->
            if uuid == model.editorModel.uuid then
                { model | editorModel = Editor.initPageModel appState subroute model.editorModel }

            else
                let
                    mbEditorUuid =
                        case subroute of
                            KMEditorRoute.Edit mbUuid ->
                                mbUuid

                            _ ->
                                Nothing
                in
                { model | editorModel = Editor.initPageModel appState subroute <| Editor.init appState uuid mbEditorUuid }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.KMEditor.Index.Models.initialModel paginationQueryString }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.KMEditor.Migration.Models.initialModel uuid }

        PublishRoute _ ->
            { model | publishModel = Wizard.KMEditor.Publish.Models.initialModel }
