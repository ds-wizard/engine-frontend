module Wizard.Public.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Auth.View
import Wizard.Public.ForgottenPassword.View
import Wizard.Public.ForgottenPasswordConfirmation.View
import Wizard.Public.Login.View
import Wizard.Public.LogoutSuccessful.View
import Wizard.Public.Models exposing (Model)
import Wizard.Public.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.View
import Wizard.Public.SignupConfirmation.View


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        AuthCallback _ _ _ _ ->
            Html.map AuthMsg <|
                Wizard.Public.Auth.View.view appState model.authModel

        ForgottenPasswordRoute ->
            Html.map ForgottenPasswordMsg <|
                Wizard.Public.ForgottenPassword.View.view appState model.forgottenPasswordModel

        ForgottenPasswordConfirmationRoute _ _ ->
            Html.map ForgottenPasswordConfirmationMsg <|
                Wizard.Public.ForgottenPasswordConfirmation.View.view appState model.forgottenPasswordConfirmationModel

        LoginRoute _ ->
            Html.map LoginMsg <|
                Wizard.Public.Login.View.view appState model.loginModel

        LogoutSuccessful ->
            Wizard.Public.LogoutSuccessful.View.view appState

        SignupRoute ->
            Html.map SignupMsg <|
                Wizard.Public.Signup.View.view appState model.signupModel

        SignupConfirmationRoute _ _ ->
            Html.map SignupConfirmationMsg <|
                Wizard.Public.SignupConfirmation.View.view appState model.signupConfirmationModel
