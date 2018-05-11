module Users.Update exposing (..)

import Auth.Models exposing (Session)
import Msgs
import Random.Pcg exposing (Seed)
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


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        CreateMsg msg ->
            let
                ( newSeed, createModel, cmd ) =
                    Users.Create.Update.update msg (wrapMsg << CreateMsg) seed session model.createModel
            in
            ( seed, { model | createModel = createModel }, cmd )

        EditMsg msg ->
            let
                ( editModel, cmd ) =
                    Users.Edit.Update.update msg (wrapMsg << EditMsg) session model.editModel
            in
            ( seed, { model | editModel = editModel }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    Users.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( seed, { model | indexModel = indexModel }, cmd )
