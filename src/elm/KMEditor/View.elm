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
import KMEditor.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ ->
            Html.map CreateMsg <|
                KMEditor.Create.View.view appState model.createModel

        EditorRoute _ ->
            Html.map EditorMsg <|
                KMEditor.Editor.View.view appState model.editorModel

        IndexRoute ->
            Html.map IndexMsg <|
                KMEditor.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                KMEditor.Migration.View.view appState model.migrationModel

        PublishRoute _ ->
            Html.map PublishMsg <|
                KMEditor.Publish.View.view appState model.publishModel
