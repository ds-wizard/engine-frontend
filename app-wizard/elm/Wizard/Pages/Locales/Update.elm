module Wizard.Pages.Locales.Update exposing (fetchData, update)

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Locales.Create.Update
import Wizard.Pages.Locales.Detail.Update
import Wizard.Pages.Locales.Import.Update
import Wizard.Pages.Locales.Index.Update
import Wizard.Pages.Locales.Models exposing (Model)
import Wizard.Pages.Locales.Msgs exposing (Msg(..))
import Wizard.Pages.Locales.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute localeId ->
            Cmd.map DetailMsg <|
                Wizard.Pages.Locales.Detail.Update.fetchData localeId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.Locales.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Pages.Locales.Create.Update.update appState cMsg (wrapMsg << CreateMsg) model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Pages.Locales.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg iMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.Pages.Locales.Import.Update.update iMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.Locales.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
