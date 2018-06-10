module Public.Update exposing (..)

import Msgs
import Public.BookReference.Update
import Public.ForgottenPassword.Update
import Public.ForgottenPasswordConfirmation.Update
import Public.Login.Update
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))
import Public.Routing exposing (Route(BookReference, SignupConfirmation))
import Public.Signup.Update
import Public.SignupConfirmation.Update
import Random.Pcg exposing (Seed)


fetchData : Route -> (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
fetchData route wrapMsg =
    case route of
        BookReference uuid ->
            Public.BookReference.Update.fetchData (wrapMsg << BookReferenceMsg) uuid

        SignupConfirmation userId hash ->
            Public.SignupConfirmation.Update.fetchData (wrapMsg << SignupConfirmationMsg) userId hash

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed model =
    case msg of
        BookReferenceMsg msg ->
            let
                ( bookReferenceModel, cmd ) =
                    Public.BookReference.Update.update msg (wrapMsg << BookReferenceMsg) model.bookReferenceModel
            in
            ( seed, { model | bookReferenceModel = bookReferenceModel }, cmd )

        ForgottenPasswordMsg msg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Public.ForgottenPassword.Update.update msg (wrapMsg << ForgottenPasswordMsg) model.forgottenPasswordModel
            in
            ( seed, { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg msg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Public.ForgottenPasswordConfirmation.Update.update msg (wrapMsg << ForgottenPasswordConfirmationMsg) model.forgottenPasswordConfirmationModel
            in
            ( seed, { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

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

        SignupConfirmationMsg msg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Public.SignupConfirmation.Update.update msg model.signupConfirmationModel
            in
            ( seed, { model | signupConfirmationModel = signupConfirmationModel }, Cmd.none )
