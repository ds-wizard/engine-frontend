module Wizard.ProjectActions.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.ProjectActions.Index.Update
import Wizard.ProjectActions.Models exposing (Model)
import Wizard.ProjectActions.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map IndexMsg <|
        Wizard.ProjectActions.Index.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.ProjectActions.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
