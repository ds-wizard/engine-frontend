module Wizard.Pages.DocumentTemplateEditors.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Create.View
import Wizard.Pages.DocumentTemplateEditors.Editor.View
import Wizard.Pages.DocumentTemplateEditors.Index.View
import Wizard.Pages.DocumentTemplateEditors.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <| Wizard.Pages.DocumentTemplateEditors.Create.View.view appState model.createModel

        EditorRoute _ subroute ->
            Html.map EditorMsg <| Wizard.Pages.DocumentTemplateEditors.Editor.View.view appState subroute model.editorModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.Pages.DocumentTemplateEditors.Index.View.view appState model.indexModel
