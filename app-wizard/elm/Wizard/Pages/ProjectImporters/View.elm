module Wizard.Pages.ProjectImporters.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectImporters.Index.View
import Wizard.Pages.ProjectImporters.Models exposing (Model)
import Wizard.Pages.ProjectImporters.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.Pages.ProjectImporters.Index.View.view appState model.indexModel
