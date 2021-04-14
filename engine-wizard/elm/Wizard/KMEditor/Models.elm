module Wizard.KMEditor.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.KMEditor.Create.Models
import Wizard.KMEditor.Editor.Models
import Wizard.KMEditor.Index.Models
import Wizard.KMEditor.Migration.Models
import Wizard.KMEditor.Publish.Models
import Wizard.KMEditor.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.KMEditor.Create.Models.Model
    , editorModel : Wizard.KMEditor.Editor.Models.Model
    , indexModel : Wizard.KMEditor.Index.Models.Model
    , migrationModel : Wizard.KMEditor.Migration.Models.Model
    , publishModel : Wizard.KMEditor.Publish.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.KMEditor.Create.Models.initialModel Nothing Nothing
    , editorModel = Wizard.KMEditor.Editor.Models.initialModel Uuid.nil
    , indexModel = Wizard.KMEditor.Index.Models.initialModel PaginationQueryString.empty
    , migrationModel = Wizard.KMEditor.Migration.Models.initialModel Uuid.nil
    , publishModel = Wizard.KMEditor.Publish.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage edit ->
            { model | createModel = Wizard.KMEditor.Create.Models.initialModel selectedPackage edit }

        EditorRoute uuid ->
            if model.editorModel.kmUuid == uuid && Wizard.KMEditor.Editor.Models.containsChanges model.editorModel then
                model

            else
                { model | editorModel = Wizard.KMEditor.Editor.Models.initialModel uuid }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.KMEditor.Index.Models.initialModel paginationQueryString }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.KMEditor.Migration.Models.initialModel uuid }

        PublishRoute _ ->
            { model | publishModel = Wizard.KMEditor.Publish.Models.initialModel }
