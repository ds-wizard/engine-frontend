module DSPlanner.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import DSPlanner.Create.Update
import DSPlanner.Detail.Update
import DSPlanner.Index.Update
import DSPlanner.Models exposing (Model)
import DSPlanner.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(..))
import Models exposing (State)
import Msgs


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Model -> Cmd Msgs.Msg
fetchData route wrapMsg session model =
    case route of
        Create _ ->
            DSPlanner.Create.Update.fetchData (wrapMsg << CreateMsg) session model.createModel

        Detail uuid ->
            DSPlanner.Detail.Update.fetchData (wrapMsg << DetailMsg) session uuid

        Index ->
            DSPlanner.Index.Update.fetchData (wrapMsg << IndexMsg) session


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    DSPlanner.Create.Update.update cMsg (wrapMsg << CreateMsg) state model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    DSPlanner.Detail.Update.update dMsg (wrapMsg << DetailMsg) state model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    DSPlanner.Index.Update.update iMsg (wrapMsg << IndexMsg) state.session model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
