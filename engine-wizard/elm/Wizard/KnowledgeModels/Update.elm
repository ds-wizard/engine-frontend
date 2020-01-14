module Wizard.KnowledgeModels.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Detail.Update
import Wizard.KnowledgeModels.Import.Update
import Wizard.KnowledgeModels.Index.Update
import Wizard.KnowledgeModels.Models exposing (Model)
import Wizard.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.KnowledgeModels.Detail.Update.fetchData packageId appState

        IndexRoute ->
            Cmd.map IndexMsg <|
                Wizard.KnowledgeModels.Index.Update.fetchData appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.KnowledgeModels.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.KnowledgeModels.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.KnowledgeModels.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
