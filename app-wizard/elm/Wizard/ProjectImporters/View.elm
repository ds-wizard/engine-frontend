module Wizard.ProjectImporters.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.ProjectImporters.Index.View
import Wizard.ProjectImporters.Models exposing (Model)
import Wizard.ProjectImporters.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.ProjectImporters.Index.View.view appState model.indexModel
