module Wizard.Public.Update exposing (fetchData, update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Auth.Update
import Wizard.Public.BookReference.Update
import Wizard.Public.ForgottenPassword.Update
import Wizard.Public.ForgottenPasswordConfirmation.Update
import Wizard.Public.Login.Update
import Wizard.Public.LogoutSuccessful.Update
import Wizard.Public.Models exposing (Model)
import Wizard.Public.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.Update
import Wizard.Public.SignupConfirmation.Update


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        AuthCallback id error code sessionState ->
            Cmd.map AuthMsg <|
                Wizard.Public.Auth.Update.fetchData id error code sessionState appState

        BookReferenceRoute uuid ->
            Cmd.map BookReferenceMsg <|
                Wizard.Public.BookReference.Update.fetchData uuid appState

        LoginRoute _ ->
            Wizard.Public.Login.Update.fetchData appState

        LogoutSuccessful ->
            Wizard.Public.LogoutSuccessful.Update.fetchData appState

        SignupConfirmationRoute userId hash ->
            Cmd.map SignupConfirmationMsg <|
                Wizard.Public.SignupConfirmation.Update.fetchData userId hash appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        AuthMsg authMsg ->
            let
                ( authModel, cmd ) =
                    Wizard.Public.Auth.Update.update authMsg (wrapMsg << AuthMsg) appState model.authModel
            in
            ( { model | authModel = authModel }, cmd )

        BookReferenceMsg brMsg ->
            let
                ( bookReferenceModel, cmd ) =
                    Wizard.Public.BookReference.Update.update brMsg appState model.bookReferenceModel
            in
            ( { model | bookReferenceModel = bookReferenceModel }, cmd )

        ForgottenPasswordMsg fpMsg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Wizard.Public.ForgottenPassword.Update.update fpMsg (wrapMsg << ForgottenPasswordMsg) appState model.forgottenPasswordModel
            in
            ( { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg fpcMsg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Wizard.Public.ForgottenPasswordConfirmation.Update.update fpcMsg (wrapMsg << ForgottenPasswordConfirmationMsg) appState model.forgottenPasswordConfirmationModel
            in
            ( { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

        LoginMsg lMsg ->
            let
                ( loginModel, cmd ) =
                    Wizard.Public.Login.Update.update lMsg (wrapMsg << LoginMsg) appState model.loginModel
            in
            ( { model | loginModel = loginModel }, cmd )

        SignupMsg sMsg ->
            let
                ( signupModel, cmd ) =
                    Wizard.Public.Signup.Update.update sMsg (wrapMsg << SignupMsg) appState model.signupModel
            in
            ( { model | signupModel = signupModel }, cmd )

        SignupConfirmationMsg scMsg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Wizard.Public.SignupConfirmation.Update.update scMsg appState model.signupConfirmationModel
            in
            ( { model | signupConfirmationModel = signupConfirmationModel }, cmd )
