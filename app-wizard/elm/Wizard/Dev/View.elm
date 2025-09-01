module Wizard.Dev.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dev.Models exposing (Model)
import Wizard.Dev.Msgs exposing (Msg(..))
import Wizard.Dev.Operations.View
import Wizard.Dev.PersistentCommandsDetail.View
import Wizard.Dev.PersistentCommandsIndex.View
import Wizard.Dev.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        OperationsRoute ->
            Html.map OperationsMsg <|
                Wizard.Dev.Operations.View.view appState model.operationsModel

        PersistentCommandsDetail _ ->
            Html.map PersistentCommandsDetailMsg <|
                Wizard.Dev.PersistentCommandsDetail.View.view appState model.persistentCommandsDetailModel

        PersistentCommandsIndex _ _ ->
            Html.map PersistentCommandsIndexMsg <|
                Wizard.Dev.PersistentCommandsIndex.View.view appState model.persistentCommandsIndexModel
