module Wizard.KMEditor.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Create.View
import Wizard.KMEditor.Editor.View
import Wizard.KMEditor.Index.View
import Wizard.KMEditor.Migration.View
import Wizard.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Publish.View
import Wizard.KMEditor.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <|
                Wizard.KMEditor.Create.View.view appState model.createModel

        EditorRoute _ ->
            Html.map EditorMsg <|
                Wizard.KMEditor.Editor.View.view appState model.editorModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.KMEditor.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.KMEditor.Migration.View.view appState model.migrationModel

        PublishRoute _ ->
            Html.map PublishMsg <|
                Wizard.KMEditor.Publish.View.view appState model.publishModel
