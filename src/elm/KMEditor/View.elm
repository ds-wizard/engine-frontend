module KMEditor.View exposing (..)

import Auth.Models exposing (JwtToken)
import Html exposing (Html)
import KMEditor.Create.View
import KMEditor.Editor.View
import KMEditor.Index.View
import KMEditor.Migration.View
import KMEditor.Models exposing (Model)
import KMEditor.Msgs exposing (Msg(..))
import KMEditor.Publish.View
import KMEditor.Routing exposing (Route(..))
import Msgs


view : Route -> (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view route wrapMsg maybeJwt model =
    case route of
        Create _ ->
            KMEditor.Create.View.view (wrapMsg << CreateMsg) model.createModel

        Editor _ ->
            KMEditor.Editor.View.view (wrapMsg << EditorMsg) model.editorModel

        Index ->
            KMEditor.Index.View.view (wrapMsg << IndexMsg) maybeJwt model.indexModel

        Migration _ ->
            KMEditor.Migration.View.view (wrapMsg << MigrationMsg) model.migrationModel

        Publish _ ->
            KMEditor.Publish.View.view (wrapMsg << PublishMsg) model.publishModel
