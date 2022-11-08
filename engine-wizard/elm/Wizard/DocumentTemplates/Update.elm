module Wizard.DocumentTemplates.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplates.Detail.Update
import Wizard.DocumentTemplates.Import.Update
import Wizard.DocumentTemplates.Index.Update
import Wizard.DocumentTemplates.Models exposing (Model)
import Wizard.DocumentTemplates.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.DocumentTemplates.Detail.Update.fetchData packageId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.DocumentTemplates.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.DocumentTemplates.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.DocumentTemplates.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( appState.seed, { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.DocumentTemplates.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
