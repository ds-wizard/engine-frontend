module Questionnaires.Update exposing (fetchData, update)

import Common.AppState exposing (AppState)
import Msgs
import Questionnaires.Create.Update
import Questionnaires.Detail.Update
import Questionnaires.Edit.Update
import Questionnaires.Index.Update
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> AppState -> Model -> Cmd Msgs.Msg
fetchData route wrapMsg appState model =
    case route of
        Create _ ->
            Questionnaires.Create.Update.fetchData (wrapMsg << CreateMsg) appState model.createModel

        Detail uuid ->
            Questionnaires.Detail.Update.fetchData (wrapMsg << DetailMsg) appState uuid

        Edit uuid ->
            Questionnaires.Edit.Update.fetchData (wrapMsg << EditMsg) appState uuid

        Index ->
            Questionnaires.Index.Update.fetchData (wrapMsg << IndexMsg) appState


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Questionnaires.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Questionnaires.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        EditMsg eMsg ->
            let
                ( editModel, cmd ) =
                    Questionnaires.Edit.Update.update eMsg (wrapMsg << EditMsg) appState model.editModel
            in
            ( { model | editModel = editModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Questionnaires.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
