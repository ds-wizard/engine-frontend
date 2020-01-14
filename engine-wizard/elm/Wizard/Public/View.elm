module Wizard.Public.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.BookReference.View
import Wizard.Public.ForgottenPassword.View
import Wizard.Public.ForgottenPasswordConfirmation.View
import Wizard.Public.Login.View
import Wizard.Public.Models exposing (Model)
import Wizard.Public.Msgs exposing (Msg(..))
import Wizard.Public.Questionnaire.View
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.View
import Wizard.Public.SignupConfirmation.View


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        BookReferenceRoute _ ->
            Html.map BookReferenceMsg <|
                Wizard.Public.BookReference.View.view appState model.bookReferenceModel

        ForgottenPasswordRoute ->
            Html.map ForgottenPasswordMsg <|
                Wizard.Public.ForgottenPassword.View.view appState model.forgottenPasswordModel

        ForgottenPasswordConfirmationRoute _ _ ->
            Html.map ForgottenPasswordConfirmationMsg <|
                Wizard.Public.ForgottenPasswordConfirmation.View.view appState model.forgottenPasswordConfirmationModel

        LoginRoute ->
            Html.map LoginMsg <|
                Wizard.Public.Login.View.view appState model.loginModel

        QuestionnaireRoute ->
            Html.map QuestionnaireMsg <|
                Wizard.Public.Questionnaire.View.view appState model.questionnaireModel

        SignupRoute ->
            Html.map SignupMsg <|
                Wizard.Public.Signup.View.view appState model.signupModel

        SignupConfirmationRoute _ _ ->
            Html.map SignupConfirmationMsg <|
                Wizard.Public.SignupConfirmation.View.view appState model.signupConfirmationModel
