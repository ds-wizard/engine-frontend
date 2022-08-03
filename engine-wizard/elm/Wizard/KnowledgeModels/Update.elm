module Wizard.KnowledgeModels.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Detail.Update
import Wizard.KnowledgeModels.Import.Update
import Wizard.KnowledgeModels.Index.Update
import Wizard.KnowledgeModels.Models exposing (Model)
import Wizard.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Preview.Update
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.KnowledgeModels.Detail.Update.fetchData packageId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.KnowledgeModels.Index.Update.fetchData

        PreviewRoute packageId _ ->
            Cmd.map PreviewMsg <|
                Wizard.KnowledgeModels.Preview.Update.fetchData appState packageId

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.KnowledgeModels.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.KnowledgeModels.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( appState.seed, { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.KnowledgeModels.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        PreviewMsg pMsg ->
            let
                ( newSeed, projectModel, cmd ) =
                    Wizard.KnowledgeModels.Preview.Update.update pMsg (wrapMsg << PreviewMsg) appState model.previewModel
            in
            ( newSeed, { model | previewModel = projectModel }, cmd )
