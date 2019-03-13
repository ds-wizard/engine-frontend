module KnowledgeModels.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import KnowledgeModels.Detail.Update
import KnowledgeModels.Import.Update
import KnowledgeModels.Index.Update
import KnowledgeModels.Models exposing (Model)
import KnowledgeModels.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))
import Models exposing (State)
import Msgs


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Detail organizationId kmId ->
            KnowledgeModels.Detail.Update.fetchData (wrapMsg << DetailMsg) organizationId kmId session

        Index ->
            KnowledgeModels.Index.Update.fetchData (wrapMsg << IndexMsg) session

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    KnowledgeModels.Detail.Update.update dMsg (wrapMsg << DetailMsg) state model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    KnowledgeModels.Import.Update.update impMsg (wrapMsg << ImportMsg) state model.importModel
            in
            ( { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    KnowledgeModels.Index.Update.update iMsg (wrapMsg << IndexMsg) state.session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
