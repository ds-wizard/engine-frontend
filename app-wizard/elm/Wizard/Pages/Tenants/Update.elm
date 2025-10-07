module Wizard.Pages.Tenants.Update exposing
    ( fetchData
    , update
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Tenants.Create.Update
import Wizard.Pages.Tenants.Detail.Update
import Wizard.Pages.Tenants.Index.Update
import Wizard.Pages.Tenants.Models exposing (Model)
import Wizard.Pages.Tenants.Msgs exposing (Msg(..))
import Wizard.Pages.Tenants.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        CreateRoute ->
            Cmd.map CreateMsg <|
                Wizard.Pages.Tenants.Create.Update.fetchData

        IndexRoute _ _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.Tenants.Index.Update.fetchData

        DetailRoute uuid ->
            Cmd.map DetailMsg <|
                Wizard.Pages.Tenants.Detail.Update.fetchData appState uuid


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.Tenants.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )

        CreateMsg createMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Pages.Tenants.Create.Update.update appState createMsg (wrapMsg << CreateMsg) model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Pages.Tenants.Detail.Update.update detailMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )
