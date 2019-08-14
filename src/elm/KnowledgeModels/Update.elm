module KnowledgeModels.Update exposing (fetchData, update)

import Common.AppState exposing (AppState)
import KnowledgeModels.Detail.Update
import KnowledgeModels.Import.Update
import KnowledgeModels.Index.Update
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routes exposing (Route(..))
import Msgs


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                KnowledgeModels.Detail.Update.fetchData packageId appState

        IndexRoute ->
            Cmd.map IndexMsg <|
                KnowledgeModels.Index.Update.fetchData appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    KnowledgeModels.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    KnowledgeModels.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    KnowledgeModels.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
