module Public.View exposing (view)

import Common.AppState exposing (AppState)
import Html exposing (Html)
import Msgs
import Public.BookReference.View
import Public.ForgottenPassword.View
import Public.ForgottenPasswordConfirmation.View
import Public.Login.View
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))
import Public.Questionnaire.View
import Public.Routing exposing (Route(..))
import Public.Signup.View
import Public.SignupConfirmation.View


view : Route -> (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view route wrapMsg appState model =
    case route of
        BookReference uuid ->
            Public.BookReference.View.view model.bookReferenceModel

        ForgottenPassword ->
            Public.ForgottenPassword.View.view (wrapMsg << ForgottenPasswordMsg) model.forgottenPasswordModel

        ForgottenPasswordConfirmation userId hash ->
            Public.ForgottenPasswordConfirmation.View.view (wrapMsg << ForgottenPasswordConfirmationMsg) model.forgottenPasswordConfirmationModel

        Login ->
            Public.Login.View.view (wrapMsg << LoginMsg) model.loginModel

        Questionnaire ->
            Public.Questionnaire.View.view (wrapMsg << QuestionnaireMsg) appState model.questionnaireModel

        Signup ->
            Public.Signup.View.view (wrapMsg << SignupMsg) model.signupModel

        SignupConfirmation userId hash ->
            Public.SignupConfirmation.View.view model.signupConfirmationModel
