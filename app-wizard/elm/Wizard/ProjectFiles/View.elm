module Wizard.ProjectFiles.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.ProjectFiles.Index.View
import Wizard.ProjectFiles.Models exposing (Model)
import Wizard.ProjectFiles.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <| Wizard.ProjectFiles.Index.View.view appState model.indexModel
