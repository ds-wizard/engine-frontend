module Wizard.Pages.KMEditor.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Create.View
import Wizard.Pages.KMEditor.Editor.View
import Wizard.Pages.KMEditor.Index.View
import Wizard.Pages.KMEditor.Migration.View
import Wizard.Pages.KMEditor.Models exposing (Model)
import Wizard.Pages.KMEditor.Msgs exposing (Msg(..))
import Wizard.Pages.KMEditor.Publish.View
import Wizard.Pages.KMEditor.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <|
                Wizard.Pages.KMEditor.Create.View.view appState model.createModel

        EditorRoute _ subroute ->
            Html.map EditorMsg <|
                Wizard.Pages.KMEditor.Editor.View.view subroute appState model.editorModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.Pages.KMEditor.Index.View.view appState model.indexModel

        MigrationRoute _ ->
            Html.map MigrationMsg <|
                Wizard.Pages.KMEditor.Migration.View.view appState model.migrationModel

        PublishRoute _ ->
            Html.map PublishMsg <|
                Wizard.Pages.KMEditor.Publish.View.view appState model.publishModel
