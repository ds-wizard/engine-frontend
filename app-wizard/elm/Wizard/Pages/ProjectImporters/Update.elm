module Wizard.Pages.ProjectImporters.Update exposing (fetchData, update)

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.ProjectImporters.Index.Update
import Wizard.Pages.ProjectImporters.Models exposing (Model)
import Wizard.Pages.ProjectImporters.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map IndexMsg <|
        Wizard.Pages.ProjectImporters.Index.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.ProjectImporters.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
