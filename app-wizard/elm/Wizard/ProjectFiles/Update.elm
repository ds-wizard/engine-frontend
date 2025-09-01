module Wizard.ProjectFiles.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.ProjectFiles.Index.Update
import Wizard.ProjectFiles.Models exposing (Model)
import Wizard.ProjectFiles.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map IndexMsg <|
        Wizard.ProjectFiles.Index.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.ProjectFiles.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
