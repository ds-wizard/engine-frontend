module UserManagement.Update exposing (..)

import Auth.Models exposing (Session)
import Msgs
import Random.Pcg exposing (Seed)
import UserManagement.Create.Update
import UserManagement.Edit.Update
import UserManagement.Index.Update
import UserManagement.Models exposing (Model)
import UserManagement.Msgs exposing (Msg(..))
import UserManagement.Routing exposing (Route(..))


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Edit uuid ->
            UserManagement.Edit.Update.fetchData (wrapMsg << EditMsg) session uuid

        Index ->
            UserManagement.Index.Update.fetchData (wrapMsg << IndexMsg) session

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        CreateMsg msg ->
            let
                ( newSeed, createModel, cmd ) =
                    UserManagement.Create.Update.update msg (wrapMsg << CreateMsg) seed session model.createModel
            in
            ( seed, { model | createModel = createModel }, cmd )

        EditMsg msg ->
            let
                ( editModel, cmd ) =
                    UserManagement.Edit.Update.update msg (wrapMsg << EditMsg) session model.editModel
            in
            ( seed, { model | editModel = editModel }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    UserManagement.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( seed, { model | indexModel = indexModel }, cmd )
