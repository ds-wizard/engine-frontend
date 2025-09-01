module Wizard.Pages.DocumentTemplates.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Detail.View
import Wizard.Pages.DocumentTemplates.Import.View
import Wizard.Pages.DocumentTemplates.Index.View
import Wizard.Pages.DocumentTemplates.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        DetailRoute _ ->
            Html.map DetailMsg <| Wizard.Pages.DocumentTemplates.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.Pages.DocumentTemplates.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.Pages.DocumentTemplates.Index.View.view appState model.indexModel
