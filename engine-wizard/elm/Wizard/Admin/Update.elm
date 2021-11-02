module Wizard.Admin.Update exposing
    ( fetchData
    , update
    )

import Wizard.Admin.Models exposing (Model)
import Wizard.Admin.Msgs exposing (Msg(..))
import Wizard.Admin.Operations.Update
import Wizard.Admin.Routes exposing (Route(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        OperationsRoute ->
            Cmd.map OperationsMsg <|
                Wizard.Admin.Operations.Update.fetchData appState


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        OperationsMsg operationsMsg ->
            let
                ( operationsModel, cmd ) =
                    Wizard.Admin.Operations.Update.update operationsMsg (wrapMsg << OperationsMsg) appState model.operationsModel
            in
            ( { model | operationsModel = operationsModel }, cmd )
