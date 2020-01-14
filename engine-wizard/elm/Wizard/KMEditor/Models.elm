module Wizard.KMEditor.Models exposing (Model, initLocalModel, initialModel)

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
    { createModel = Wizard.KMEditor.Create.Models.initialModel Nothing
    , editorModel = Wizard.KMEditor.Editor.Models.initialModel ""
    , indexModel = Wizard.KMEditor.Index.Models.initialModel
    , migrationModel = Wizard.KMEditor.Migration.Models.initialModel ""
    , publishModel = Wizard.KMEditor.Publish.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = Wizard.KMEditor.Create.Models.initialModel selectedPackage }

        EditorRoute uuid ->
            if model.editorModel.kmUuid == uuid && Wizard.KMEditor.Editor.Models.containsChanges model.editorModel then
                model

            else
                { model | editorModel = Wizard.KMEditor.Editor.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = Wizard.KMEditor.Index.Models.initialModel }

        MigrationRoute uuid ->
            { model | migrationModel = Wizard.KMEditor.Migration.Models.initialModel uuid }

        PublishRoute _ ->
            { model | publishModel = Wizard.KMEditor.Publish.Models.initialModel }
