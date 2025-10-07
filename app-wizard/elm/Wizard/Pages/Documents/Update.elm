module Wizard.Pages.Documents.Update exposing
    ( fetchData
    , update
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Documents.Index.Update
import Wizard.Pages.Documents.Models exposing (Model)
import Wizard.Pages.Documents.Msgs exposing (Msg(..))


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    Cmd.map IndexMsg <|
        Wizard.Pages.Documents.Index.Update.fetchData appState model.indexModel


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.Documents.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
