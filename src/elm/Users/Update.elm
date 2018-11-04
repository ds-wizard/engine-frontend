module Users.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Models exposing (State)
import Msgs
import Random exposing (Seed)
import Users.Create.Update
import Users.Edit.Update
import Users.Index.Update
import Users.Models exposing (Model)
import Users.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Edit uuid ->
            Users.Edit.Update.fetchData (wrapMsg << EditMsg) session uuid

        Index ->
            Users.Index.Update.fetchData (wrapMsg << IndexMsg) session

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        CreateMsg createMsg ->
            let
                ( newSeed, createModel, cmd ) =
                    Users.Create.Update.update createMsg (wrapMsg << CreateMsg) state model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Users.Edit.Update.update editMsg (wrapMsg << EditMsg) state.session model.editModel
            in
            ( state.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Users.Index.Update.update indexMsg (wrapMsg << IndexMsg) state.session model.indexModel
            in
            ( state.seed, { model | indexModel = indexModel }, cmd )
