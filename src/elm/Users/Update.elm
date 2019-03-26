module Users.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.AppState exposing (AppState)
import Msgs
import Random exposing (Seed)
import Users.Create.Update
import Users.Edit.Update
import Users.Index.Update
import Users.Models exposing (Model)
import Users.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData route wrapMsg appState =
    case route of
        Edit uuid ->
            Users.Edit.Update.fetchData (wrapMsg << EditMsg) appState uuid

        Index ->
            Users.Index.Update.fetchData (wrapMsg << IndexMsg) appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg createMsg ->
            let
                ( newSeed, createModel, cmd ) =
                    Users.Create.Update.update createMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Users.Edit.Update.update editMsg (wrapMsg << EditMsg) appState model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Users.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
