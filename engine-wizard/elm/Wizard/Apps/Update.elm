module Wizard.Apps.Update exposing
    ( fetchData
    , update
    )

import Wizard.Apps.Create.Update
import Wizard.Apps.Detail.Update
import Wizard.Apps.Index.Update
import Wizard.Apps.Models exposing (Model)
import Wizard.Apps.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (Route(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Apps.Index.Update.fetchData

        DetailRoute uuid ->
            Cmd.map DetailMsg <|
                Wizard.Apps.Detail.Update.fetchData appState uuid

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Apps.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )

        CreateMsg createMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Apps.Create.Update.update appState createMsg (wrapMsg << CreateMsg) model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Apps.Detail.Update.update detailMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )
