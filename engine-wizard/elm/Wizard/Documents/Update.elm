module Wizard.Documents.Update exposing (..)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Index.Update
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Documents.Index.Update.fetchData appState model.indexModel


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Documents.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
