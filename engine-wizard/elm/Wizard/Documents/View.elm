module Wizard.Documents.View exposing (..)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Index.View
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        IndexRoute _ _ ->
            Html.map IndexMsg <|
                Wizard.Documents.Index.View.view appState model.indexModel
