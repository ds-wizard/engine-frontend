module Wizard.Pages.ProjectFiles.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectFiles.Index.View
import Wizard.Pages.ProjectFiles.Models exposing (Model)
import Wizard.Pages.ProjectFiles.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.Pages.ProjectFiles.Index.View.view appState model.indexModel
