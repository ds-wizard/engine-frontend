module Wizard.Pages.Dev.Update exposing
    ( fetchData
    , update
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dev.Models exposing (Model)
import Wizard.Pages.Dev.Msgs exposing (Msg(..))
import Wizard.Pages.Dev.Operations.Update
import Wizard.Pages.Dev.PersistentCommandsDetail.Update
import Wizard.Pages.Dev.PersistentCommandsIndex.Update
import Wizard.Pages.Dev.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        OperationsRoute ->
            Cmd.map OperationsMsg <|
                Wizard.Pages.Dev.Operations.Update.fetchData appState

        PersistentCommandsDetail uuid ->
            Cmd.map PersistentCommandsDetailMsg <|
                Wizard.Pages.Dev.PersistentCommandsDetail.Update.fetchData appState uuid

        PersistentCommandsIndex _ _ ->
            Cmd.map PersistentCommandsIndexMsg <|
                Wizard.Pages.Dev.PersistentCommandsIndex.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        OperationsMsg operationsMsg ->
            let
                ( operationsModel, cmd ) =
                    Wizard.Pages.Dev.Operations.Update.update operationsMsg (wrapMsg << OperationsMsg) appState model.operationsModel
            in
            ( { model | operationsModel = operationsModel }, cmd )

        PersistentCommandsDetailMsg pcdMsg ->
            let
                ( persistentCommandsDetailModel, cmd ) =
                    Wizard.Pages.Dev.PersistentCommandsDetail.Update.update pcdMsg (wrapMsg << PersistentCommandsDetailMsg) appState model.persistentCommandsDetailModel
            in
            ( { model | persistentCommandsDetailModel = persistentCommandsDetailModel }, cmd )

        PersistentCommandsIndexMsg pciMsg ->
            let
                ( persistentCommandsIndexModel, cmd ) =
                    Wizard.Pages.Dev.PersistentCommandsIndex.Update.update pciMsg (wrapMsg << PersistentCommandsIndexMsg) appState model.persistentCommandsIndexModel
            in
            ( { model | persistentCommandsIndexModel = persistentCommandsIndexModel }, cmd )
