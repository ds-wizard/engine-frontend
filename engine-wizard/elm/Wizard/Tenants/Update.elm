module Wizard.Tenants.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Tenants.Create.Update
import Wizard.Tenants.Detail.Update
import Wizard.Tenants.Index.Update
import Wizard.Tenants.Models exposing (Model)
import Wizard.Tenants.Msgs exposing (Msg(..))
import Wizard.Tenants.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Tenants.Index.Update.fetchData

        DetailRoute uuid ->
            Cmd.map DetailMsg <|
                Wizard.Tenants.Detail.Update.fetchData appState uuid

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Tenants.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )

        CreateMsg createMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Tenants.Create.Update.update appState createMsg (wrapMsg << CreateMsg) model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Tenants.Detail.Update.update detailMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )
