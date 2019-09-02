module KMEditor.Models exposing (Model, initLocalModel, initialModel)

import KMEditor.Create.Models
import KMEditor.Editor.Models
import KMEditor.Index.Models
import KMEditor.Migration.Models
import KMEditor.Publish.Models
import KMEditor.Routes exposing (Route(..))


type alias Model =
    { createModel : KMEditor.Create.Models.Model
    , editorModel : KMEditor.Editor.Models.Model
    , indexModel : KMEditor.Index.Models.Model
    , migrationModel : KMEditor.Migration.Models.Model
    , publishModel : KMEditor.Publish.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = KMEditor.Create.Models.initialModel Nothing
    , editorModel = KMEditor.Editor.Models.initialModel ""
    , indexModel = KMEditor.Index.Models.initialModel
    , migrationModel = KMEditor.Migration.Models.initialModel ""
    , publishModel = KMEditor.Publish.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = KMEditor.Create.Models.initialModel selectedPackage }

        EditorRoute uuid ->
            if model.editorModel.kmUuid == uuid && KMEditor.Editor.Models.containsChanges model.editorModel then
                model

            else
                { model | editorModel = KMEditor.Editor.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = KMEditor.Index.Models.initialModel }

        MigrationRoute uuid ->
            { model | migrationModel = KMEditor.Migration.Models.initialModel uuid }

        PublishRoute _ ->
            { model | publishModel = KMEditor.Publish.Models.initialModel }
