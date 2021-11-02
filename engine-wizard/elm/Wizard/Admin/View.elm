module Wizard.Admin.View exposing (view)

import Html exposing (Html)
import Wizard.Admin.Models exposing (Model)
import Wizard.Admin.Msgs exposing (Msg(..))
import Wizard.Admin.Operations.View
import Wizard.Admin.Routes exposing (Route(..))
import Wizard.Common.AppState exposing (AppState)


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        OperationsRoute ->
            Html.map OperationsMsg <|
                Wizard.Admin.Operations.View.view appState model.operationsModel
