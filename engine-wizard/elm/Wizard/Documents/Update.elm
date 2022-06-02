module Wizard.Documents.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Index.Update
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
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
