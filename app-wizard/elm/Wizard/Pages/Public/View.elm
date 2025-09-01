module Wizard.Pages.Public.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Auth.View
import Wizard.Pages.Public.ForgottenPassword.View
import Wizard.Pages.Public.ForgottenPasswordConfirmation.View
import Wizard.Pages.Public.Login.View
import Wizard.Pages.Public.LogoutSuccessful.View
import Wizard.Pages.Public.Models exposing (Model)
import Wizard.Pages.Public.Msgs exposing (Msg(..))
import Wizard.Pages.Public.Routes exposing (Route(..))
import Wizard.Pages.Public.Signup.View
import Wizard.Pages.Public.SignupConfirmation.View


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        AuthCallback _ _ _ _ ->
            Html.map AuthMsg <|
                Wizard.Pages.Public.Auth.View.view appState model.authModel

        ForgottenPasswordRoute ->
            Html.map ForgottenPasswordMsg <|
                Wizard.Pages.Public.ForgottenPassword.View.view appState model.forgottenPasswordModel

        ForgottenPasswordConfirmationRoute _ _ ->
            Html.map ForgottenPasswordConfirmationMsg <|
                Wizard.Pages.Public.ForgottenPasswordConfirmation.View.view appState model.forgottenPasswordConfirmationModel

        LoginRoute _ ->
            Html.map LoginMsg <|
                Wizard.Pages.Public.Login.View.view appState model.loginModel

        LogoutSuccessful ->
            Wizard.Pages.Public.LogoutSuccessful.View.view appState

        SignupRoute ->
            Html.map SignupMsg <|
                Wizard.Pages.Public.Signup.View.view appState model.signupModel

        SignupConfirmationRoute _ _ ->
            Html.map SignupConfirmationMsg <|
                Wizard.Pages.Public.SignupConfirmation.View.view appState model.signupConfirmationModel
