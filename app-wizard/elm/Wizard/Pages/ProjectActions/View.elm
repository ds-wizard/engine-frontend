module Wizard.Pages.ProjectActions.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectActions.Index.View
import Wizard.Pages.ProjectActions.Models exposing (Model)
import Wizard.Pages.ProjectActions.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.Pages.ProjectActions.Index.View.view appState model.indexModel
