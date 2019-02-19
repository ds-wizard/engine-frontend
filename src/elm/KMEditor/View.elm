module KMEditor.View exposing (view)

import Auth.Models exposing (JwtToken)
import Html exposing (Html)
import KMEditor.Create.View
import KMEditor.Editor.View
import KMEditor.Index.View
import KMEditor.Migration.View
import KMEditor.Models exposing (Model)
import KMEditor.Msgs exposing (Msg(..))
import KMEditor.Preview.View
import KMEditor.Publish.View
import KMEditor.Routing exposing (Route(..))
import KMEditor.TagEditor.View
import Msgs


view : Route -> (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view route wrapMsg maybeJwt model =
    case route of
        CreateRoute _ ->
            KMEditor.Create.View.view (wrapMsg << CreateMsg) model.createModel

        EditorRoute _ ->
            KMEditor.Editor.View.view (wrapMsg << EditorMsg) model.editorModel

        IndexRoute ->
            KMEditor.Index.View.view (wrapMsg << IndexMsg) maybeJwt model.indexModel

        MigrationRoute _ ->
            KMEditor.Migration.View.view (wrapMsg << MigrationMsg) model.migrationModel

        PreviewRoute _ ->
            KMEditor.Preview.View.view (wrapMsg << PreviewMsg) model.previewModel

        PublishRoute _ ->
            KMEditor.Publish.View.view (wrapMsg << PublishMsg) model.publishModel

        TagEditorRoute _ ->
            KMEditor.TagEditor.View.view (wrapMsg << TagEditorMsg) model.tagEditorModel
