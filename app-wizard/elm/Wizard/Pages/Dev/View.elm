module Wizard.Pages.Dev.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Models exposing (Model)
import Wizard.Pages.Dev.Msgs exposing (Msg(..))
import Wizard.Pages.Dev.Operations.View
import Wizard.Pages.Dev.PersistentCommandsDetail.View
import Wizard.Pages.Dev.PersistentCommandsIndex.View
import Wizard.Pages.Dev.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        OperationsRoute ->
            Html.map OperationsMsg <|
                Wizard.Pages.Dev.Operations.View.view appState model.operationsModel

        PersistentCommandsDetail _ ->
            Html.map PersistentCommandsDetailMsg <|
                Wizard.Pages.Dev.PersistentCommandsDetail.View.view appState model.persistentCommandsDetailModel

        PersistentCommandsIndex _ _ ->
            Html.map PersistentCommandsIndexMsg <|
                Wizard.Pages.Dev.PersistentCommandsIndex.View.view appState model.persistentCommandsIndexModel
