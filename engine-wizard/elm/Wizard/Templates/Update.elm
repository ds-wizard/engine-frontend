module Wizard.Templates.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Templates.Detail.Update
import Wizard.Templates.Import.Update
import Wizard.Templates.Index.Update
import Wizard.Templates.Models exposing (Model)
import Wizard.Templates.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.Templates.Detail.Update.fetchData packageId appState

        IndexRoute ->
            Cmd.map IndexMsg <|
                Wizard.Templates.Index.Update.fetchData appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Templates.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.Templates.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Templates.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
