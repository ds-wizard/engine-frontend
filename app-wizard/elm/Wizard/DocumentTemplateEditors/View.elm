module Wizard.DocumentTemplateEditors.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Create.View
import Wizard.DocumentTemplateEditors.Editor.View
import Wizard.DocumentTemplateEditors.Index.View
import Wizard.DocumentTemplateEditors.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg <| Wizard.DocumentTemplateEditors.Create.View.view appState model.createModel

        EditorRoute _ subroute ->
            Html.map EditorMsg <| Wizard.DocumentTemplateEditors.Editor.View.view appState subroute model.editorModel

        IndexRoute _ ->
            Html.map IndexMsg <|
                Wizard.DocumentTemplateEditors.Index.View.view appState model.indexModel
