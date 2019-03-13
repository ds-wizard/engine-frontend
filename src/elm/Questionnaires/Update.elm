module Questionnaires.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Models exposing (State)
import Msgs
import Questionnaires.Create.Update
import Questionnaires.Detail.Update
import Questionnaires.Index.Update
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Model -> Cmd Msgs.Msg
fetchData route wrapMsg session model =
    case route of
        Create _ ->
            Questionnaires.Create.Update.fetchData (wrapMsg << CreateMsg) session model.createModel

        Detail uuid ->
            Questionnaires.Detail.Update.fetchData (wrapMsg << DetailMsg) session uuid

        Index ->
            Questionnaires.Index.Update.fetchData (wrapMsg << IndexMsg) session


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Questionnaires.Create.Update.update cMsg (wrapMsg << CreateMsg) state model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Questionnaires.Detail.Update.update dMsg (wrapMsg << DetailMsg) state model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Questionnaires.Index.Update.update iMsg (wrapMsg << IndexMsg) state.session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
