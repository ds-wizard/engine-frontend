module Questionnaires.Update exposing (..)

import Auth.Models exposing (Session)
import Msgs
import Questionnaires.Create.Update
import Questionnaires.Detail.Update
import Questionnaires.Index.Update
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Create ->
            Questionnaires.Create.Update.fetchData (wrapMsg << CreateMsg) session

        Detail uuid ->
            Questionnaires.Detail.Update.fetchData (wrapMsg << DetailMsg) session uuid

        Index ->
            Questionnaires.Index.Update.fetchData (wrapMsg << IndexMsg) session


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        CreateMsg msg ->
            let
                ( createModel, cmd ) =
                    Questionnaires.Create.Update.update msg (wrapMsg << CreateMsg) session model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg msg ->
            let
                ( detailModel, cmd ) =
                    Questionnaires.Detail.Update.update msg (wrapMsg << DetailMsg) session model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    Questionnaires.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
