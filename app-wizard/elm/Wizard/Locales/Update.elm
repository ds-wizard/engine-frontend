module Wizard.Locales.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Create.Update
import Wizard.Locales.Detail.Update
import Wizard.Locales.Import.Update
import Wizard.Locales.Index.Update
import Wizard.Locales.Models exposing (Model)
import Wizard.Locales.Msgs exposing (Msg(..))
import Wizard.Locales.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute localeId ->
            Cmd.map DetailMsg <|
                Wizard.Locales.Detail.Update.fetchData localeId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Locales.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Locales.Create.Update.update appState cMsg (wrapMsg << CreateMsg) model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Locales.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg iMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.Locales.Import.Update.update iMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Locales.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
