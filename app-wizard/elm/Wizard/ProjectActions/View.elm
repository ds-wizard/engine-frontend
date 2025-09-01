module Wizard.ProjectActions.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.ProjectActions.Index.View
import Wizard.ProjectActions.Models exposing (Model)
import Wizard.ProjectActions.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.ProjectActions.Index.View.view appState model.indexModel
