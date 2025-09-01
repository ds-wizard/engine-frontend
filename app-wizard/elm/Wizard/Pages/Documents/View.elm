module Wizard.Pages.Documents.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Documents.Index.View
import Wizard.Pages.Documents.Models exposing (Model)
import Wizard.Pages.Documents.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Html.map IndexMsg <|
        Wizard.Pages.Documents.Index.View.view appState model.indexModel
