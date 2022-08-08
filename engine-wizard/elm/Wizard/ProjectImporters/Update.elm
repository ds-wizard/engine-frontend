module Wizard.ProjectImporters.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.ProjectImporters.Index.Update
import Wizard.ProjectImporters.Models exposing (Model)
import Wizard.ProjectImporters.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map IndexMsg <|
        Wizard.ProjectImporters.Index.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.ProjectImporters.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
