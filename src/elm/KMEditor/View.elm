module KMEditor.View exposing (view)

import Common.AppState exposing (AppState)
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


view : Route -> (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view route wrapMsg appState model =
    case route of
        CreateRoute _ ->
            KMEditor.Create.View.view (wrapMsg << CreateMsg) model.createModel

        EditorRoute _ ->
            KMEditor.Editor.View.view (wrapMsg << EditorMsg) appState model.editorModel

        IndexRoute ->
            KMEditor.Index.View.view (wrapMsg << IndexMsg) appState.jwt model.indexModel

        MigrationRoute _ ->
            KMEditor.Migration.View.view (wrapMsg << MigrationMsg) model.migrationModel

        PublishRoute _ ->
            Html.map (wrapMsg << PublishMsg) <|
                KMEditor.Publish.View.view model.publishModel
