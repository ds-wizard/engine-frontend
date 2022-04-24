module Wizard.Dev.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Dev.Models exposing (Model)
import Wizard.Dev.Msgs exposing (Msg(..))
import Wizard.Dev.Operations.Update
import Wizard.Dev.PersistentCommandsDetail.Update
import Wizard.Dev.PersistentCommandsIndex.Update
import Wizard.Dev.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        OperationsRoute ->
            Cmd.map OperationsMsg <|
                Wizard.Dev.Operations.Update.fetchData appState

        PersistentCommandsDetail uuid ->
            Cmd.map PersistentCommandsDetailMsg <|
                Wizard.Dev.PersistentCommandsDetail.Update.fetchData appState uuid

        PersistentCommandsIndex _ _ ->
            Cmd.map PersistentCommandsIndexMsg <|
                Wizard.Dev.PersistentCommandsIndex.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        OperationsMsg operationsMsg ->
            let
                ( operationsModel, cmd ) =
                    Wizard.Dev.Operations.Update.update operationsMsg (wrapMsg << OperationsMsg) appState model.operationsModel
            in
            ( { model | operationsModel = operationsModel }, cmd )

        PersistentCommandsDetailMsg pcdMsg ->
            let
                ( persistentCommandsDetailModel, cmd ) =
                    Wizard.Dev.PersistentCommandsDetail.Update.update pcdMsg (wrapMsg << PersistentCommandsDetailMsg) appState model.persistentCommandsDetailModel
            in
            ( { model | persistentCommandsDetailModel = persistentCommandsDetailModel }, cmd )

        PersistentCommandsIndexMsg pciMsg ->
            let
                ( persistentCommandsIndexModel, cmd ) =
                    Wizard.Dev.PersistentCommandsIndex.Update.update pciMsg (wrapMsg << PersistentCommandsIndexMsg) appState model.persistentCommandsIndexModel
            in
            ( { model | persistentCommandsIndexModel = persistentCommandsIndexModel }, cmd )
