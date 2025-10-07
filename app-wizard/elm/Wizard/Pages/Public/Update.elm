module Wizard.Pages.Public.Update exposing (fetchData, update)

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Public.Auth.Update
import Wizard.Pages.Public.ForgottenPassword.Update
import Wizard.Pages.Public.ForgottenPasswordConfirmation.Update
import Wizard.Pages.Public.Login.Update
import Wizard.Pages.Public.LogoutSuccessful.Update
import Wizard.Pages.Public.Models exposing (Model)
import Wizard.Pages.Public.Msgs exposing (Msg(..))
import Wizard.Pages.Public.Routes exposing (Route(..))
import Wizard.Pages.Public.Signup.Update
import Wizard.Pages.Public.SignupConfirmation.Update


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        AuthCallback id error code sessionState ->
            Cmd.map AuthMsg <|
                Wizard.Pages.Public.Auth.Update.fetchData id error code sessionState appState

        LoginRoute mbOriginalUrl ->
            Wizard.Pages.Public.Login.Update.fetchData appState mbOriginalUrl

        LogoutSuccessful ->
            Wizard.Pages.Public.LogoutSuccessful.Update.fetchData appState

        SignupConfirmationRoute userId hash ->
            Cmd.map SignupConfirmationMsg <|
                Wizard.Pages.Public.SignupConfirmation.Update.fetchData userId hash appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        AuthMsg authMsg ->
            let
                ( authModel, cmd ) =
                    Wizard.Pages.Public.Auth.Update.update authMsg (wrapMsg << AuthMsg) appState model.authModel
            in
            ( { model | authModel = authModel }, cmd )

        ForgottenPasswordMsg fpMsg ->
            let
                ( forgottenPasswordModel, cmd ) =
                    Wizard.Pages.Public.ForgottenPassword.Update.update fpMsg (wrapMsg << ForgottenPasswordMsg) appState model.forgottenPasswordModel
            in
            ( { model | forgottenPasswordModel = forgottenPasswordModel }, cmd )

        ForgottenPasswordConfirmationMsg fpcMsg ->
            let
                ( forgottenPasswordConfirmationModel, cmd ) =
                    Wizard.Pages.Public.ForgottenPasswordConfirmation.Update.update fpcMsg (wrapMsg << ForgottenPasswordConfirmationMsg) appState model.forgottenPasswordConfirmationModel
            in
            ( { model | forgottenPasswordConfirmationModel = forgottenPasswordConfirmationModel }, cmd )

        LoginMsg lMsg ->
            let
                ( loginModel, cmd ) =
                    Wizard.Pages.Public.Login.Update.update lMsg (wrapMsg << LoginMsg) appState model.loginModel
            in
            ( { model | loginModel = loginModel }, cmd )

        SignupMsg sMsg ->
            let
                ( signupModel, cmd ) =
                    Wizard.Pages.Public.Signup.Update.update sMsg (wrapMsg << SignupMsg) appState model.signupModel
            in
            ( { model | signupModel = signupModel }, cmd )

        SignupConfirmationMsg scMsg ->
            let
                ( signupConfirmationModel, cmd ) =
                    Wizard.Pages.Public.SignupConfirmation.Update.update scMsg appState model.signupConfirmationModel
            in
            ( { model | signupConfirmationModel = signupConfirmationModel }, cmd )
