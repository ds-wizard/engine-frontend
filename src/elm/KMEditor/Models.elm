module KMEditor.Models exposing (Model, initLocalModel, initialModel)

import KMEditor.Create.Models
import KMEditor.Editor.Models
import KMEditor.Index.Models
import KMEditor.Migration.Models
import KMEditor.Preview.Models
import KMEditor.Publish.Models
import KMEditor.Routing exposing (Route(..))
import KMEditor.TagEditor.Models


type alias Model =
    { createModel : KMEditor.Create.Models.Model
    , editorModel : KMEditor.Editor.Models.Model
    , indexModel : KMEditor.Index.Models.Model
    , migrationModel : KMEditor.Migration.Models.Model
    , previewModel : KMEditor.Preview.Models.Model
    , publishModel : KMEditor.Publish.Models.Model
    , tagEditorModel : KMEditor.TagEditor.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = KMEditor.Create.Models.initialModel Nothing
    , editorModel = KMEditor.Editor.Models.initialModel ""
    , indexModel = KMEditor.Index.Models.initialModel
    , migrationModel = KMEditor.Migration.Models.initialModel ""
    , previewModel = KMEditor.Preview.Models.initialModel ""
    , publishModel = KMEditor.Publish.Models.initialModel
    , tagEditorModel = KMEditor.TagEditor.Models.initialModel ""
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selectedPackage ->
            { model | createModel = KMEditor.Create.Models.initialModel selectedPackage }

        EditorRoute uuid ->
            if model.editorModel.branchUuid == uuid then
                model

            else
                { model | editorModel = KMEditor.Editor.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = KMEditor.Index.Models.initialModel }

        MigrationRoute uuid ->
            { model | migrationModel = KMEditor.Migration.Models.initialModel uuid }

        PreviewRoute uuid ->
            { model | previewModel = KMEditor.Preview.Models.initialModel uuid }

        PublishRoute uuid ->
            { model | publishModel = KMEditor.Publish.Models.initialModel }

        TagEditorRoute uuid ->
            if model.tagEditorModel.branchUuid == uuid then
                model

            else
                { model | tagEditorModel = KMEditor.TagEditor.Models.initialModel uuid }
