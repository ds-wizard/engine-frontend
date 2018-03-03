module Public.Update exposing (..)

import Msgs
import Public.Login.Update
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))
import Public.Signup.Update
import Random.Pcg exposing (Seed)


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed model =
    case msg of
        LoginMsg msg ->
            let
                ( loginModel, cmd ) =
                    Public.Login.Update.update msg (wrapMsg << LoginMsg) model.loginModel
            in
            ( seed, { model | loginModel = loginModel }, cmd )

        SignupMsg msg ->
            let
                ( newSeed, signupModel, cmd ) =
                    Public.Signup.Update.update msg (wrapMsg << SignupMsg) seed model.signupModel
            in
            ( newSeed, { model | signupModel = signupModel }, cmd )

        _ ->
            ( seed, model, Cmd.none )
