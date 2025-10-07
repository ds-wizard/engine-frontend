module Wizard.Pages.DocumentTemplates.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Detail.Update
import Wizard.Pages.DocumentTemplates.Import.Update
import Wizard.Pages.DocumentTemplates.Index.Update
import Wizard.Pages.DocumentTemplates.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.Pages.DocumentTemplates.Detail.Update.fetchData packageId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.DocumentTemplates.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Pages.DocumentTemplates.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.Pages.DocumentTemplates.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( appState.seed, { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.DocumentTemplates.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
