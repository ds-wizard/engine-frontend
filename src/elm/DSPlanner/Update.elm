module DSPlanner.Update exposing (..)

import Auth.Models exposing (Session)
import DSPlanner.Create.Update
import DSPlanner.Detail.Update
import DSPlanner.Index.Update
import DSPlanner.Models exposing (Model)
import DSPlanner.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(..))
import Msgs


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Create ->
            DSPlanner.Create.Update.fetchData (wrapMsg << CreateMsg) session

        Detail uuid ->
            DSPlanner.Detail.Update.fetchData (wrapMsg << DetailMsg) session uuid

        Index ->
            DSPlanner.Index.Update.fetchData (wrapMsg << IndexMsg) session


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        CreateMsg msg ->
            let
                ( createModel, cmd ) =
                    DSPlanner.Create.Update.update msg (wrapMsg << CreateMsg) session model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg msg ->
            let
                ( detailModel, cmd ) =
                    DSPlanner.Detail.Update.update msg (wrapMsg << DetailMsg) session model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    DSPlanner.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
